module OpenAPI.Checker.Validate where

import           Control.Monad
import           Data.Functor
import           Data.HashMap.Strict.InsOrd     as InsMap
import           Data.HashSet.InsOrd            as InsSet
import           Data.OpenApi
import           Data.OpenApi.Internal
import           Data.Traversable
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate.Monad

reportCompat :: OpenApi -> OpenApi -> Report
reportCompat = error "FIXME: reportCompat not implemented"

forwardCompatible :: OpenApi -> OpenApi -> ReportTree
forwardCompatible dec enc = fst $ runTreeM $ openApiCompatible dec enc

openApiCompatible :: OpenApi -> OpenApi -> TreeM ReportTree ()
openApiCompatible dec enc = void $ follow $
  (InsMap.toList $ _openApiPaths dec) <&> \(path, encItem) ->
  (path, checkItem path encItem)
  where
    -- checkItem :: Path -> PathItem -> TreeM PathItemTree ()
    checkItem path encItem = do
      case (InsMap.lookup path $ _openApiPaths enc) of
        Nothing      -> pathError $ "Path deleted"
        Just decItem -> pathItemsCompatible encItem decItem

pathItemsCompatible :: PathItem -> PathItem -> TreeM PathItemTree ()
pathItemsCompatible dec enc = void $ follow
  [ (Get, go _pathItemGet)
  , (Put, go _pathItemPut)
  , (Post, go _pathItemPost)
  , (Delete, go _pathItemDelete)
  , (Options, go _pathItemOptions)
  , (Head, go _pathItemHead)
  , (Patch, go _pathItemPatch)
  , (Trace, go _pathItemTrace) ]
  where
    go :: (PathItem -> Maybe Operation) -> TreeM OperationTree ()
    go getOp = (error "FIXME: not implemented")
