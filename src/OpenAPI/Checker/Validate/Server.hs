{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Server
  ( CheckIssue (..)
  )
where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Data.Attoparsec.Text
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHS
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import Prelude as P

tracedParsedServerUrlParts
  :: Traced' r [Server] Server
  -> Traced' r ProcessedServer (Either (CheckIssue ProcessedServer) ProcessedServer)
tracedParsedServerUrlParts s =
  let rawURL = _serverUrl $ extract s
      parsedUrl = parseServerUrl rawURL
      serverVariables = _serverVariables $ extract s
   in traced (ask s >>> step (ServerStep rawURL)) $
        parsedUrl
          & (traverse . traverse)
            (\var -> case IOHM.lookup var serverVariables of
               Nothing -> Left VariableNotDefined
               Just x -> Right x)

instance Subtree [Server] where
  type CheckEnv [Server] = '[]
  data CheckIssue [Server]
    deriving (Eq, Ord, Show)
  checkCompatibility env pcServer = do
    let (ProdCons (pErrs, pUrls) (cErrs, cUrls)) =
          pcServer <&> partitionEithers . fmap (bicosequence . tracedParsedServerUrlParts) . sequence
        bicosequence :: Comonad f => f (Either a b) -> Either (f a) (f b)
        bicosequence x = case extract x of
          Left e -> Left $ x $> e
          Right a -> Right $ x $> a
        throwAllErrors = traverse_ tracedIssue
    throwAllErrors pErrs
    throwAllErrors cErrs
    for_ pUrls $ \cUrl -> do
      let potentiallyCompatible = P.filter ((staticCompatible `on` extract) cUrl) cUrls
      anyOfSubtreeAt cUrl ServerNotMatched $ potentiallyCompatible <&> (checkCompatibility env . ProdCons cUrl)
    pure ()

type ProcessedServer = [ServerUrlPart ServerVariable]

-- | Nothing means "open variable" – can have any value
unifyPart :: ServerUrlPart ServerVariable -> Maybe (IOHS.InsOrdHashSet Text)
unifyPart (ServerUrlVariable v) = _serverVariableEnum v
unifyPart (ServerUrlConstant c) = Just $ IOHS.singleton c

zipAll :: [a] -> [b] -> Maybe [(a, b)]
zipAll [] [] = Just []
zipAll (x : xs) (y : ys) = ((x, y) :) <$> zipAll xs ys
zipAll (_ : _) [] = Nothing
zipAll [] (_ : _) = Nothing

staticCompatiblePart :: ServerUrlPart x -> ServerUrlPart x -> Bool
staticCompatiblePart (ServerUrlConstant x) (ServerUrlConstant y) = x == y
staticCompatiblePart _ _ = True

staticCompatible :: [ServerUrlPart x] -> [ServerUrlPart x] -> Bool
staticCompatible a b = maybe False (all $ uncurry staticCompatiblePart) $ zipAll a b

data ServerUrlPart var
  = ServerUrlVariable var
  | ServerUrlConstant Text
  deriving stock (Show, Functor, Foldable, Traversable)

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

instance Subtree ProcessedServer where
  type CheckEnv ProcessedServer = '[]
  data CheckIssue ProcessedServer
    = VariableNotDefined
    | ServerNotMatched
    | EnumValueNotConsumed Int Text
    | ConsumerNotOpen Int
    deriving (Eq, Ord, Show)
  checkCompatibility _ pc@(ProdCons p _) =
    -- traversing here is fine because we have already filtered for length
    for_ (zip [0 ..] $ zipProdCons . fmap (fmap unifyPart . extract) $ pc) $ \(i, pcPart) -> case pcPart of
      (Just x, Just y) -> for_ x $ \v -> unless (v `IOHS.member` y) (issueAt p $ EnumValueNotConsumed i v)
      -- Consumer can consume anything
      (_, Nothing) -> pure ()
      -- Producer can produce anythings, but consumer has a finite enum ;(
      (Nothing, Just _) -> issueAt p (ConsumerNotOpen i)
    where
      zipProdCons :: ProdCons [a] -> [(a, a)]
      zipProdCons (ProdCons x y) = zip x y
