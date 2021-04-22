{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Server
  ( CheckIssue(..)
  ) where

import Control.Applicative
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
import Prelude as P

instance Subtree [Server] where
  type CheckEnv [Server] = '[]
  data CheckIssue [Server]
    = VariableNotDefined Text
    | EnumValueNotPresent Int Text
    | ConsumerNotOpen Int
    | ServerNotMatched Int
    deriving (Eq, Ord, Show)
  checkCompatibility _ pcServer = do
    let urls =
          traverse sequenceA $ do
            ss <- pcServer
            f <- prodConsAccessors
            pure $
              ss
                <&> (\s ->
                       let rawUrl = parseServerUrl . _serverUrl $ s
                           vars = _serverVariables s
                        in rawUrl
                             & (traverse . traverse)
                               (\var -> case IOHM.lookup var vars of
                                  Nothing -> Left $ SomeIssue f (VariableNotDefined var)
                                  Just x -> Right x))
    case urls of
      Left e -> issue e
      Right ProdCons {producer = ps, consumer = cs} -> do
        let ps' =
              ps <&> \p ->
                ( unifyPart <$> p
                , P.filter (maybe False (all (uncurry staticCompatible)) . zipAll p) cs
                )
        for_ (zip [0 ..] ps') $ \(i, (p, cs')) ->
          anyOfSubtreeAt producer (ServerNotMatched i) $
            cs' <&> \c ->
              -- we can just 'zip' here because length was already filtered out
              for_ (zip p (unifyPart <$> c)) $ \case
                (Just x, Just y) ->
                  for_ x $ \v -> unless (v `IOHS.member` y) (issueAt consumer $ EnumValueNotPresent i v)
                -- Consumer can consume anything
                (_, Nothing) -> pure ()
                -- Producer can produce anythings, but consumer has a finite enum ;(
                (Nothing, Just _) -> issueAt consumer (ConsumerNotOpen i)

-- | Nothing means "open variable" – can have any value
unifyPart :: ServerUrlPart ServerVariable -> Maybe (IOHS.InsOrdHashSet Text)
unifyPart (ServerUrlVariable v) = _serverVariableEnum $ v
unifyPart (ServerUrlConstant c) = Just $ IOHS.singleton c

zipAll :: [a] -> [b] -> Maybe [(a, b)]
zipAll [] [] = Just []
zipAll (x : xs) (y : ys) = ((x, y) :) <$> zipAll xs ys
zipAll (_ : _) [] = Nothing
zipAll [] (_ : _) = Nothing

staticCompatible :: ServerUrlPart x -> ServerUrlPart x -> Bool
staticCompatible (ServerUrlConstant x) (ServerUrlConstant y) = x == y
staticCompatible _ _ = True

data ServerUrlPart var
  = ServerUrlVariable var
  | ServerUrlConstant Text
  deriving stock (Show, Functor, Foldable, Traversable)

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
