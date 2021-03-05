module OpenAPI.Checker.Validate.Dereference where

import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.Generics.Product
import           Data.OpenApi.Internal
import           OpenAPI.Checker.Validate.Monad

-- | Throws error if param not found
dereferenceOldParam :: Referenced Param -> TreeM t Param
dereferenceOldParam = \case
  Ref r -> do
    env <- ask
    lookupParam (env ^. field @"roots" . field @"old") r
  Inline a -> pure a

dereferenceNewParam :: Referenced Param -> TreeM t Param
dereferenceNewParam = \case
  Ref r -> do
    env <- ask
    lookupParam (env ^. field @"roots" . field @"new") r
  Inline a -> pure a

parseReference :: Reference -> [Text]
parseReference (Reference r) = T.pack . unesc . T.unpack  <$> T.splitOn "/" r
  where
    unesc = \case
      '~' : '0' : rest -> '~' : unesc rest
      '~' : '1' : rest -> '/' : unesc rest
      s : rest -> s : unesc rest
      [] -> []

lookupParam :: OpenApi -> [Text] -> TreeM t Param
lookupParam api = \case
  ""
