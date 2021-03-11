module OpenAPI.Checker.Validate where

import           Control.Monad.Reader
import           Data.Functor
import           Data.HashMap.Strict.InsOrd           as InsMap
import           Data.Map.Strict                      as M
import           Data.OpenApi
import           OpenAPI.Checker.Aux
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate.Dereference
import           OpenAPI.Checker.Validate.Monad

reportCompat :: OpenApi -> OpenApi -> Report
reportCompat = error "FIXME: reportCompat not implemented"

forwardCompatible :: OpenApi -> OpenApi -> Errorable ReportTree
forwardCompatible old new
  = fmap snd
  $ runTreeM emptyEnv
  $ openApiCompatible old new

openApiCompatible :: OpenApi -> OpenApi -> TreeM ReportTree ()
openApiCompatible old new = do
  local (const servers) $ do
    follow $ (InsMap.toList $ _openApiPaths old) <&> \(path, oldItem) ->
      (path, checkItem path oldItem)
  where
    servers = Env
      { oldServers = _openApiServers old
      , newServers = _openApiServers new }
    checkItem path oldItem = do
      case (InsMap.lookup path $ _openApiPaths new) of
        Nothing      -> treeError $ "Path deleted"
        Just newItem ->
          pathItemsCompatible oldItem newItem


mergeEnvServers :: [Server] -> [Server] -> TreeM t Env
mergeEnvServers olds news = do
  env <- ask
  return $ Env
    { oldServers = mergeServers (oldServers env) olds
    , newServers = mergeServers (newServers env) news }

pathItemsCompatible
  :: PathItem
  -> PathItem
  -> TreeM PathItemTree ()
pathItemsCompatible old new = do
  newEnv <- mergeEnvServers (_pathItemServers old) (_pathItemServers new)
  local (const newEnv) $ do
    follow
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
    go getOp = case getOp old of
      Nothing -> return ()
      -- Operation is not in old schema, so any addition is Ok
      Just oldOp -> case getOp new of
        Nothing -> do
        -- Operation was in old schema, but no in new one.
          treeError "Operation was deleted. Old client may try to use it."
        Just newOp ->
          operationsCompatible oldOp newOp

operationsCompatible
  :: Operation
  -> Operation
  -> TreeM OperationTree ()
operationsCompatible old new = do
  -- There seems to be some weirdness with an Error instance being required for some reason.
  -- Since it doesn't work anyways, this should be fine for now.
  oldParams <- maybe undefined pure $ traverse (dereferenceParam undefined) $ _operationParameters old
  newParams <- maybe undefined pure $ traverse (dereferenceParam undefined) $ _operationParameters new
  let
    newPMap = M.fromList $ newParams <&> \p -> (getParamKey p, p)
    checkParam pkey oldP = case M.lookup pkey newPMap of
      Nothing ->
        treeError "Param was deleted in new interface, but exists in old"
      Just newP -> do
        paramsCompatible oldP newP
  follow $ oldParams <&> \oldP ->
    let pkey = getParamKey oldP
    in (pkey , checkParam pkey oldP)

paramsCompatible :: Param -> Param -> TreeM ParamTree ()
paramsCompatible = error "FIXME: paramsCompatible not implemented"
