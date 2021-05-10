{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module OpenAPI.Checker.Validate.Operation
  ( MatchedOperation (..)
  , OperationMethod(..)
  , pathItemMethod
  ) where


import Data.Foldable as F
import Data.Functor
import Data.HList
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.PathFragment
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.RequestBody ()
import OpenAPI.Checker.Validate.Responses ()
import OpenAPI.Checker.Validate.Server ()
import qualified Data.OpenApi.Schema.Generator as G
import OpenAPI.Checker.Common

data MatchedOperation = MatchedOperation
  { operation :: !Operation
  , pathParams :: ![Traced Param]
  -- ^ Params from the PathItem
  , getPathFragments :: !([Traced Param] -> [Traced PathFragmentParam])
  -- ^ Path fragments traced from PathItem. Takes full list of
  -- operation-specific parameters
  }

type ParamKey = (ParamLocation, Text)

paramKey :: Param -> ParamKey
paramKey param = (_paramIn param, _paramName param)

tracedParameters :: Traced MatchedOperation -> [Traced (Referenced Param)]
tracedParameters oper =
  [ traced (ask oper >>> step (OperationParamsStep i)) x
  | (i, x) <- zip [0..] $ _operationParameters . operation $ extract oper
  ]

tracedRequestBody :: Traced MatchedOperation -> Maybe (Traced (Referenced RequestBody))
tracedRequestBody oper = _operationRequestBody (operation $ extract oper) <&> traced (ask oper >>> step OperationRequestBodyStep)

tracedResponses :: Traced MatchedOperation -> Traced Responses
tracedResponses oper = traced (ask oper >>> step OperationResponsesStep)
  $ _operationResponses . operation $ extract oper

tracedSecurity :: Traced MatchedOperation -> [Traced SecurityRequirement]
tracedSecurity oper =
  [ traced (ask oper >>> step (OperationSecurityRequirementStep i)) x
  | (i, x) <- zip [0..] $ _operationSecurity . operation $ extract oper
  ]

-- FIXME: https://github.com/typeable/openapi-diff/issues/28
tracedServers
  :: [Server] -- ^ Servers from env
  -> Traced MatchedOperation
  -> Traced [Server]
tracedServers env oper =
  traced (ask oper >>> step OperationServersStep) $
    case _operationServers . operation $ extract oper of
      [] -> env
      ss -> ss

instance Behavable 'OperationLevel 'PathFragmentLevel where
  data Behave 'OperationLevel 'PathFragmentLevel
    = InParam Text
    | InFragment Int
    deriving stock (Eq, Ord, Show)

instance Behavable 'OperationLevel 'RequestLevel where
  data Behave 'OperationLevel 'RequestLevel
    = InRequest
    deriving stock (Eq, Ord, Show)

instance Subtree MatchedOperation where
  type SubtreeLevel MatchedOperation = 'OperationLevel
  type CheckEnv MatchedOperation =
    '[ ProdCons (Traced (Definitions Param))
     , ProdCons (Traced (Definitions RequestBody))
     , ProdCons (Traced (Definitions SecurityScheme))
     , ProdCons (Traced (Definitions Response))
     , ProdCons (Traced (Definitions Header))
     , ProdCons (Traced (Definitions Schema))
     , ProdCons [Server]
     ]
  checkStructuralCompatibility env pc = do
    let
      pParams :: ProdCons [Param]
      pParams = do
        defs <- extract <$> getH @(ProdCons (Traced (Definitions Param))) env
        op' <- _operationParameters . operation <$> pc
        pp <- fmap extract . pathParams <$> pc
        pure $ 
          let o = M.fromList $ do
                param <- G.dereference defs <$> op'
                let key = paramKey param
                pure (key, param)
              p = M.fromList $ do
                param <- pp
                pure (paramKey param, param)
            in M.elems $ o <> p
      reqBody = do
        defs <- extract <$> getH @(ProdCons (Traced (Definitions RequestBody))) env
        body <- _operationRequestBody . operation <$> pc
        pure $ G.dereference defs <$> body
    case zipAll (producer pParams) (consumer pParams) of
      Nothing -> absurdIssue 
      Just xs -> for_ xs $ \(p, c) -> checkStructuralCompatibility env $ ProdCons p c
    case reqBody of
      ProdCons Nothing Nothing -> pure ()
      ProdCons (Just p) (Just c) -> checkStructuralCompatibility env $ ProdCons p c
      _ -> absurdIssue
    checkStructuralCompatibility env $ _operationResponses . operation <$> pc
    checkStructuralCompatibility env $ _operationServers . operation <$> pc
    -- TODO: Callbacks
    -- TODO: Security
    pure ()
  checkSemanticCompatibility env beh prodCons = do
    checkParameters
    checkRequestBodies
    checkResponses
    checkCallbacks
    checkOperationSecurity
    checkServers
    pure ()
    where
      checkParameters = do
        let
          -- Merged parameters got from Operation and PathItem in one
          -- place. First element is path params, second is non-path params
          tracedParams :: ProdCons ([Traced Param], [Traced Param])
          tracedParams = getParams <$> paramDefs <*> prodCons
          getParams defs mp =
            let
              operationParamsMap :: Map ParamKey (Traced Param)
              operationParamsMap = M.fromList $ do
                paramRef <- tracedParameters mp
                let
                  param = dereference defs paramRef
                  key = paramKey . extract $ param
                pure (key, param)
              pathParamsMap :: Map ParamKey (Traced Param)
              pathParamsMap = M.fromList $ do
                param <- pathParams . extract $ mp
                pure (paramKey . extract $ param, param)
              params = M.elems $ M.union operationParamsMap pathParamsMap -- We prefer params from Operation
              splitted = L.partition
                (\p -> (_paramIn . extract $ p) == ParamPath) params
            in splitted
        checkNonPathParams $ snd <$> tracedParams
        checkPathParams $ fst <$> tracedParams
        pure ()
      checkNonPathParams :: ProdCons [Traced Param] -> SemanticCompatFormula ()
      checkNonPathParams params = do
        let
          elements = getEls <$> params
          getEls params = M.fromList $ do
            p <- params
            let
              k = (_paramIn . extract $ p, _paramName . extract $ p)
              v = ProductLike
                { productValue = p
                , required = fromMaybe False . _paramRequired . extract $ p
                }
            pure (k, v)
          check (_, name) param = do
            checkCompatibility @Param (singletonH schemaDefs) (beh >>> step (InParam name)) param
        checkProducts beh (ParamNotMatched . snd) check elements
      checkPathParams :: ProdCons [Traced Param] -> SemanticCompatFormula ()
      checkPathParams pathParams = do
        let
          fragments :: ProdCons [Traced PathFragmentParam]
          fragments = getFragments <$> pathParams <*> prodCons
          getFragments params mop = getPathFragments (extract mop) params
          -- Feed path parameters to the fragments getter
          check idx frags = checkCompatibility @PathFragmentParam env (beh >>> step (InFragment idx)) frags
          elements = fragments <&> \frags -> M.fromList $ zip [0 :: Int ..] $ do
            frag <- frags
            pure $ ProductLike
              { productValue = frag
              , required = True }
        checkProducts beh PathFragmentNotMatched check elements
      checkRequestBodies = do
        let
          check reqBody = checkCompatibility @RequestBody env (beh >>> step InRequest) reqBody
          elements = getReqBody <$> bodyDefs <*> prodCons
          getReqBody bodyDef mop = M.fromList $ do
            bodyRef <- F.toList . tracedRequestBody $ mop
            let
              body = dereference bodyDef bodyRef
            -- Single element map
            pure ((), ProductLike
              { productValue = body
              , required = fromMaybe False . _requestBodyRequired . extract $ body
              })
        checkProducts beh (const NoRequestBody) (const check) elements
      checkResponses = do
        let
          respEnv = HCons (swapProdCons respDefs)
            $ HCons (swapProdCons headerDefs)
            $ HCons (swapProdCons schemaDefs) HNil
          resps = tracedResponses <$> prodCons
        checkCompatibility respEnv beh $ swapProdCons resps
      -- FIXME: https://github.com/typeable/openapi-diff/issues/27
      checkCallbacks = pure () -- (error "FIXME: not implemented")
      -- FIXME: https://github.com/typeable/openapi-diff/issues/28
      checkOperationSecurity = pure () -- (error "FIXME: not implemented")
      checkServers =
        checkCompatibility env beh $
          tracedServers <$> getH @(ProdCons [Server]) env <*> prodCons
      bodyDefs = getH @(ProdCons (Traced (Definitions RequestBody))) env
      respDefs = getH @(ProdCons (Traced (Definitions Response))) env
      headerDefs =  getH @(ProdCons (Traced (Definitions Header))) env
      schemaDefs = getH @(ProdCons (Traced (Definitions Schema))) env
      paramDefs = getH @(ProdCons (Traced (Definitions Param))) env

data OperationMethod =
  GetMethod
  | PutMethod
  | PostMethod
  | DeleteMethod
  | OptionsMethod
  | HeadMethod
  | PatchMethod
  | TraceMethod
  deriving (Eq, Ord, Show)

pathItemMethod :: OperationMethod -> PathItem -> Maybe Operation
pathItemMethod = \case
  GetMethod -> _pathItemGet
  PutMethod -> _pathItemPut
  PostMethod -> _pathItemPost
  DeleteMethod -> _pathItemDelete
  OptionsMethod -> _pathItemOptions
  HeadMethod -> _pathItemHead
  PatchMethod -> _pathItemPatch
  TraceMethod -> _pathItemTrace

instance Steppable MatchedOperation (Referenced Param) where
  data Step MatchedOperation (Referenced Param) = OperationParamsStep Int
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation (Referenced RequestBody) where
  data Step MatchedOperation (Referenced RequestBody) = OperationRequestBodyStep
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation Responses where
  data Step MatchedOperation Responses = OperationResponsesStep
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation SecurityRequirement where
  data Step MatchedOperation SecurityRequirement = OperationSecurityRequirementStep Int
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation [Server] where
  data Step MatchedOperation [Server]
    = OperationServersStep
    | EnvServerStep
    deriving (Eq, Ord, Show)
