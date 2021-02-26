module OpenAPI.Checker.Validate where

import           Control.Monad
import           Data.Functor
import           Data.HashMap.Strict.InsOrd     as InsMap
import           Data.HashSet.InsOrd            as InsSet
import           Data.OpenApi
import           Data.OpenApi.Internal
import           Data.Traversable
import           OpenAPI.Checker.Aux
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate.Monad

reportCompat :: OpenApi -> OpenApi -> Report
reportCompat = error "FIXME: reportCompat not implemented"

forwardCompatible :: OpenApi -> OpenApi -> ReportTree
forwardCompatible old new = fst $ runTreeM $ openApiCompatible old new

openApiCompatible :: OpenApi -> OpenApi -> TreeM ReportTree ()
openApiCompatible old new = void $ follow $
  (InsMap.toList $ _openApiPaths old) <&> \(path, oldItem) ->
  (path, checkItem path oldItem)
  where
    oldServers = _openApiServers old
    newServers = _openApiServers new
    checkItem path oldItem = do
      case (InsMap.lookup path $ _openApiPaths new) of
        Nothing      -> treeError $ "Path deleted"
        Just newItem ->
          pathItemsCompatible (oldServers, oldItem) (newServers, newItem)

pathItemsCompatible
  :: ([Server], PathItem)
  -> ([Server], PathItem)
  -> TreeM PathItemTree ()
pathItemsCompatible (oldServers' , old) (newServers' , new) = void $ follow
  [ (Get, go _pathItemGet)
  , (Put, go _pathItemPut)
  , (Post, go _pathItemPost)
  , (Delete, go _pathItemDelete)
  , (Options, go _pathItemOptions)
  , (Head, go _pathItemHead)
  , (Patch, go _pathItemPatch)
  , (Trace, go _pathItemTrace) ]
  where
    oldServers = mergeServers oldServers' (_pathItemServers old)
    newServers = mergeServers newServers' (_pathItemServers new)
    go :: (PathItem -> Maybe Operation) -> TreeM OperationTree ()
    go getOp = case getOp old of
      Nothing -> return ()
      -- Operation is not in old schema, so any addition is Ok
      Just oldOp -> case getOp new of
        Nothing -> do
        -- Operation was in old schema, but no in new one.
          treeError "Operation was deleted. Old client may try to use it."
        Just newOp -> operationsCompatible (oldServers, oldOp) (newServers, newOp)

operationsCompatible
  :: ([Server], Operation)
  -> ([Server], Operation)
  -> TreeM OperationTree ()
operationsCompatible = error "FIXME: operationsCompatible not implemented"
