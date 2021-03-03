module OpenAPI.Checker.Validate where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Functor
import           Data.Generics.Product
import           Data.HashMap.Strict.InsOrd           as InsMap
import           Data.HashSet.InsOrd                  as InsSet
import           Data.Map.Strict                      as M
import           Data.OpenApi
import           Data.OpenApi.Internal
import           Data.Traversable
import           OpenAPI.Checker.Aux
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate.Dereference
import           OpenAPI.Checker.Validate.Monad

reportCompat :: OpenApi -> OpenApi -> Report
reportCompat = error "FIXME: reportCompat not implemented"

forwardCompatible :: OpenApi -> OpenApi -> ReportTree
forwardCompatible old new
  = snd
  $ runTreeM emptyEnv
  $ openApiCompatible old new

openApiCompatible :: OpenApi -> OpenApi -> TreeM ReportTree ()
openApiCompatible old new = do
  local (const servers) $ do
    let
      oldPaths = _openApiPaths old
      newPaths = _openApiPaths new
      common = InsMap.intersectionWith (,) oldPaths newPaths
      removed = InsMap.difference oldPaths newPaths
      added = InsMap.difference newPaths oldPaths
    for_ (InsMap.toList removed) $ \(path, oldItem) -> do
      let
        a = Final
          { compat = Incompatible
            "Path deleted. Old client may do request on it"
          , diffOp = Removed
          , original = oldItem }
      field @"paths" <>= final a
    for_ (InsMap.toList added) $ \ (path, newItem) -> do
      let
        a = Final
          { compat = Compatible
          , diffOp = Added
          , original = newItem }
      field @"paths" <>= final a
    follow_ $ InsMap.toList common <&> \(path, (oldItem, newItem)) ->
      (path, pathItemsCompatible oldItem newItem)
  where
    servers = Env
      { oldServers = _openApiServers old
      , newServers = _openApiServers new }


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
    go Get _pathItemGet
    go Put _pathItemPut
    go Post _pathItemPost
    go Delete _pathItemDelete
    go Options _pathItemOptions
    go Head _pathItemHead
    go Patch _pathItemPatch
    go Trace _pathItemTrace
  where
    go
      :: OperationName
      -> (PathItem -> Maybe Operation)
      -> TreeM PathItemTree ()
    go key getOp = case (getOp old, getOp new) of
      (Just oldOp, Just newOp) ->
        followSingle key $ operationsCompatible oldOp newOp
      (Just oldOp, Nothing) -> do
        let
          a = Final
            { compat =
              Incompatible "Operation deleted. Client may try to use it"
            , diffOp = Removed
            , original = oldOp }
        field @"operations" <>= final a
      (Nothing, Just newOp) -> do
        let
          a = Final
            { compat = Compatible
            , diffOp = Added
            , original = newOp }
        field @"operations" <>= final a
      _ -> pure ()

operationsCompatible
  :: Operation
  -> Operation
  -> TreeM OperationTree ()
operationsCompatible old new = do
  let toMap ps = M.fromList $ ps <&> \p -> (getParamKey p, p)
  oldParams <- fmap toMap $ traverse dereferenceParam $ _operationParameters old
  newParams <- fmap toMap $ traverse dereferenceParam $ _operationParameters new
  let
    common = M.intersectionWith (,) oldParams newParams
    removed = M.difference oldParams newParams
    added = M.difference newParams oldParams
  for_ (M.toList removed) $ \(key, paramItem) -> do
    let
      a = Final
        { compat = Compatible
        -- Assume removed parameters will be ignored by server
        , diffOp = Removed
        , original = paramItem }
    field @"parameters" <>= final a
  for_ (M.toList added) $ \(key, paramItem) -> do
    let
      a = Final
        { compat
        , diffOp = Added
        , original = paramItem }
      compat = case _paramRequired paramItem of
        Just False -> Compatible
        -- Assume absent optional parameter is OK
        _ -> Incompatible "Added parameter. Client may not know about it"
    field @"parameters" <>= final a
  follow_ $ M.toList common <&> \ (key, (oldParam, newParam)) ->
    (key, paramsCompatible oldParam newParam)

paramsCompatible :: Param -> Param -> TreeM ParamTree ()
paramsCompatible = error "FIXME: paramsCompatible not implemented"
