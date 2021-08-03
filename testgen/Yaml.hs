module Yaml
  ( Reference (..)
  , readYamlTree
  ) where

import Data.Aeson
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Yaml

data Reference = Unlabelled String | Labelled String String
  deriving stock (Eq, Ord, Show)

readYamlTree :: MonadIO m => (Reference -> m FilePath) -> FilePath -> m Value
readYamlTree resolve root = liftIO (decodeFileThrow root) >>= go
  where
    go (Object kv) = Object <$> mapM go kv
    go (Array xs) = Array <$> mapM go xs
    go (String str) | Just ('$', kv) <- T.uncons str = resolve (ref kv) >>= liftIO . decodeFileThrow >>= go
      where
        ref kv | (key, T.uncons -> Just ('=', value)) <- T.break (== '=') kv = Labelled (T.unpack key) (T.unpack value)
               | otherwise = Unlabelled (T.unpack kv)
    go value = pure value
