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
import           Data.Set                             as S
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
  local (const env) $ do
    let
      oldPaths = _openApiPaths old
      newPaths = _openApiPaths new
      commonPaths = InsMap.intersectionWith (,) oldPaths newPaths
      removedPaths = InsMap.difference oldPaths newPaths
      addedPaths = InsMap.difference newPaths oldPaths
    for_ (InsMap.toList removedPaths) $ \(path, oldItem) -> do
      let
        a = Final
          { compat = Incompatible
            "Path deleted. Old client may do request on it"
          , diffOp = Removed
          , original = oldItem }
      field @"paths" <>= final a
    for_ (InsMap.toList addedPaths) $ \ (path, newItem) -> do
      let
        a = Final
          { compat = Compatible
          , diffOp = Added
          , original = newItem }
      field @"paths" <>= final a
    follow_ $ InsMap.toList commonPaths <&> \(path, (oldItem, newItem)) ->
      (path, pathItemsCompatible oldItem newItem)
    let
      oldSecs = S.fromList
        $ (InsMap.toHashMap . getSecurityRequirement) <$> _openApiSecurity old
      newSecs = S.fromList
        $ (InsMap.toHashMap . getSecurityRequirement) <$> _openApiSecurity new
      addedSecs = S.difference newSecs oldSecs
      removedSecs = S.difference oldSecs newSecs
    for_ (S.toList addedSecs) $ \sec -> do
      let
        a = Final
          { compat = Incompatible
            "Added new security requirement which may be ommited by client."
          , diffOp = Added
          , original = sec }
      field @"securityRequirements" <>= pure a
    for_ (S.toList removedSecs) $ \sec -> do
      let
        a = Final
          { compat = Compatible
          -- Assume dropping security requirements is OK
          , diffOp = Removed
          , original = sec }
      field @"securityRequirements" <>= pure a
  where
    env = Env
      { servers = OldNew
        { old = fromServers $ _openApiServers old
        , new = fromServers $ _openApiServers new
        }
      , parameters = mempty
      }

mergeEnvServers :: OldNew [Server] -> TreeM t a -> TreeM t a
mergeEnvServers (OldNew olds news) ma = do
  let
    env = Env
      { servers = OldNew
        { old = fromServers olds
        , new = fromServers news
        }
      , parameters = mempty
      }
  local (<> env) ma

pathItemsCompatible
  :: PathItem
  -> PathItem
  -> TreeM PathItemTree ()
pathItemsCompatible old new = do
  mergeEnvServers (OldNew (_pathItemServers old) (_pathItemServers new)) $ do
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
