module Yaml
  ( readYamlTree
  ) where

import Data.Char
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Yaml.Parser
import Data.Yaml.Internal
import Text.Libyaml

-- copied from Data.Yaml.Internal
textToValue :: Style -> Tag -> T.Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t = String t
textToValue Folded _ t = String t
textToValue _ _ t
  | t `elem` ["null", "Null", "NULL", "~", ""] = Null
  | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
  | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
  | Right x <- textToScientific t = Number x
  | otherwise = String t
  where
    x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
      where titleCased = toUpper (T.head ref) `T.cons` T.tail ref

readYamlTree :: MonadIO m => (String -> m FilePath) -> FilePath -> m Value
readYamlTree resolve root = liftIO (readYamlFile root) >>= go
  where
    go (Mapping kv mAnchor) = noAnchor mAnchor $ object <$> mapM (mapM go) kv
    go (Sequence xs mAnchor) = noAnchor mAnchor $ listValue id <$> mapM go xs
    go (Scalar bs tag style mAnchor) = noAnchor mAnchor $ pure $ textToValue style tag (decodeUtf8With lenientDecode bs)
    go (Alias anchor) = resolve anchor >>= liftIO . readYamlFile >>= go

    noAnchor (Just anchor) _ = throw (FromYamlException $ "Unexpected anchor binding: " <> T.pack anchor)
    noAnchor Nothing act = act
