{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Server
  ( Issue (..)
  )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Comonad
import Control.Monad
import Data.Attoparsec.Text
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHS
import Data.Maybe
import Data.OpenApi
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Common
import OpenAPI.Checker.Paths
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.MediaTypeObject
import Prelude as P

tracedParsedServerUrlParts
  :: Server
  -> Either (Issue 'ServerLevel) ProcessedServer
tracedParsedServerUrlParts s =
  let parsedUrl = parseServerUrl $ _serverUrl s
      lookupVar var = case IOHM.lookup var (_serverVariables s) of
        Nothing -> Left $ ServerVariableNotDefined var
        Just x -> Right x
   in (traverse @[] . traverse @ServerUrlPart) lookupVar parsedUrl

instance Behavable 'OperationLevel 'ServerLevel where
  data Behave 'OperationLevel 'ServerLevel
    = InServer Text
    deriving stock (Eq, Ord, Show)

instance Subtree [Server] where
  type SubtreeLevel [Server] = 'OperationLevel
  type CheckEnv [Server] = '[]
  checkStructuralCompatibility _ pc =
    structuralEq $ fmap S.fromList . (fmap . fmap) reduceServer <$> pc
    where
      reducerServerVariable =
        fmap IOHM.toHashSet . _serverVariableEnum &&& _serverVariableDefault
      reduceServer =
        _serverUrl &&& fmap reducerServerVariable . IOHM.toHashMap . _serverVariables
  checkSemanticCompatibility env beh pcServer = do
    let (ProdCons (pErrs, pUrls) (cErrs, cUrls)) =
          pcServer
            <&> partitionEithers
              . fmap
                (\(Traced t s) ->
                   let bhv = beh >>> step (InServer $ _serverUrl s)
                    in case tracedParsedServerUrlParts s of
                         Left e -> Left $ issueAt bhv e
                         Right u -> Right (bhv, Traced (t >>> step (ServerStep $ _serverUrl s)) u))
              . sequence
    sequenceA_ pErrs
    sequenceA_ cErrs
    for_ pUrls $ \(bhv, pUrl) -> do
      let potentiallyCompatible = P.filter ((staticCompatible `on` extract) pUrl) $ fmap snd cUrls
      anyOfAt
        bhv
        ServerNotMatched
        [ checkCompatibility env bhv (ProdCons pUrl cUrl)
        | cUrl <- potentiallyCompatible
        ]
    pure ()

type ProcessedServer = [ServerUrlPart ServerVariable]

-- | Nothing means "open variable" – can have any value
unifyPart :: ServerUrlPart ServerVariable -> Maybe (IOHS.InsOrdHashSet Text)
unifyPart (ServerUrlVariable v) = _serverVariableEnum v
unifyPart (ServerUrlConstant c) = Just $ IOHS.singleton c

staticCompatiblePart :: ServerUrlPart x -> ServerUrlPart x -> Bool
staticCompatiblePart (ServerUrlConstant x) (ServerUrlConstant y) = x == y
staticCompatiblePart _ _ = True

staticCompatible :: [ServerUrlPart x] -> [ServerUrlPart x] -> Bool
staticCompatible a b = maybe False (all $ uncurry staticCompatiblePart) $ zipAll a b

data ServerUrlPart var
  = ServerUrlVariable var
  | ServerUrlConstant Text
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | This is super rough. Things like @{a|b}c@ will not match @ac@.
-- FIXME: https://github.com/typeable/openapi-diff/issues/46
--
-- NOTE: syntax is defined vaguely in the spec.
parseServerUrl :: Text -> [ServerUrlPart Text]
-- There really is no way it can fail
parseServerUrl = fromRight undefined . parseOnly (serverUrlParser <* endOfInput)
  where
    serverUrlParser :: Parser [ServerUrlPart Text]
    serverUrlParser = many $ do
      variableUrlParser <|> do
        a <- anyChar
        aa <- takeTill (== '{')
        return (ServerUrlConstant $ T.cons a aa)

    variableUrlParser :: Parser (ServerUrlPart Text)
    variableUrlParser = do
      char '{'
      res <- takeTill (== '}')
      char '}'
      return $ ServerUrlVariable res

instance Steppable [Server] ProcessedServer where
  data Step [Server] ProcessedServer = ServerStep Text
    deriving (Eq, Ord, Show)

instance Issuable 'ServerLevel where
  data Issue 'ServerLevel
    = EnumValueNotConsumed Int Text
    | ConsumerNotOpen Int
    | ServerVariableNotDefined Text
    | ServerNotMatched
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Subtree ProcessedServer where
  type SubtreeLevel ProcessedServer = 'ServerLevel
  type CheckEnv ProcessedServer = '[]
  checkStructuralCompatibility _ pc =
    structuralEq $ (fmap . fmap . fmap . fmap) reducerServerVariable pc
    where
      reducerServerVariable =
        fmap IOHM.toHashSet . _serverVariableEnum &&& _serverVariableDefault
  checkSemanticCompatibility _ beh pc =
    -- traversing here is fine because we have already filtered for length
    for_ (zip [0 ..] $ zipProdCons . fmap (fmap unifyPart . extract) $ pc) $ \(i, pcPart) -> case pcPart of
      (Just x, Just y) -> for_ x $ \v -> unless (v `IOHS.member` y) (issueAt beh $ EnumValueNotConsumed i v)
      -- Consumer can consume anything
      (_, Nothing) -> pure ()
      -- Producer can produce anythings, but consumer has a finite enum ;(
      (Nothing, Just _) -> issueAt beh (ConsumerNotOpen i)
    where
      zipProdCons :: ProdCons [a] -> [(a, a)]
      zipProdCons (ProdCons x y) = zip x y
