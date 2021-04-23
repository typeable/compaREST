{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Operation
  ( MatchedOperation(..)
  ) where


import Data.Foldable as F
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Param ()
import OpenAPI.Checker.Validate.PathFragment
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.RequestBody ()
import OpenAPI.Checker.Validate.Responses ()
import OpenAPI.Checker.Validate.SecurityRequirement ()
import OpenAPI.Checker.Validate.Server ()

-- data ParamKey

data MatchedOperation = MatchedOperation
  { operation :: Operation
  , pathParams :: [Traced OpenApi Param ]
  -- ^ Params from the PathItem
  }

-- | Normalized key for matching parameters. If keys of two parameters are equal
-- then parameters must be checked for compatibility
data ParamKey
  = PathPosition Int
  | OtherLocation ParamLocation Text
  deriving (Eq, Ord, Show)

instance Subtree MatchedOperation where
  type CheckEnv MatchedOperation =
    '[ ProdCons (Definitions Param)
     , ProdCons (Definitions RequestBody)
     , ProdCons (Definitions SecurityScheme)
     , ProdCons (Definitions Response)
     , ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue MatchedOperation
    = ParamNotMatched ParamLocation Text -- Param name
    | NoRequestBody
    | CallbacksNotSupported
    | SecurityRequirementNotMet Int -- security indexs
    | ServerNotConsumed Int -- server index
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons@(ProdCons p c) = do
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
          tracedParams :: ProdCons ([Traced OpenApi Param], [Traced OpenApi Param])
          tracedParams = getParams <$> prodCons
          getParams mp =
            let
              operationParamsMap :: Map (ParamLocation, Text) (Traced OpenApi Param)
              operationParamsMap = (error "FIXME: not implemented")
              pathParamsMap :: Map (ParamLocation, Text) (Traced OpenApi Param)
              pathParamsMap = error "FIXME: pathParamsMap not implemented"
              params = M.elems $ M.union operationParamsMap pathParamsMap
              -- We prefer params from Operation
              splitted = L.partition
                (\p -> (_paramIn $ getTraced p) == ParamPath) params
            in splitted
        checkNonPathParams $ snd <$> tracedParams
        checkPathParams $ fst <$> tracedParams
        pure ()
      checkNonPathParams :: ProdCons [Traced OpenApi Param] -> CompatFormula MatchedOperation ()
      checkNonPathParams params = do
        let
          elements = getEls <$> params
          getEls traced = M.fromList $ do
            p <- traced
            let
              param = getTraced p
              k = (_paramIn param, _paramName param)
              v = ProductLike
                { traced = p
                , required = fromMaybe False $ _paramRequired param
                }
            pure (k, v)
          check _ param = do
            checkCompatibility @Param (singletonH schemaDefs) param
        checkProducts' (uncurry ParamNotMatched) check elements
      checkPathParams params = (error "FIXME: not implemented")
      checkRequestBodies = do
        let
          check _ reqBody = checkCompatibility @RequestBody env reqBody
          elements = getReqBody <$> bodyDefs <*> prodCons
          getReqBody bodyDef mop = M.fromList $ do
            bodyRef <- F.toList $ _operationRequestBody $ operation mop
            let
              traced = dereferenceTraced bodyDef
                $ Traced (step $ OperationRequestBodyStep) bodyRef
              required = fromMaybe False
                $ _requestBodyRequired $ getTraced traced
              elt = ProductLike { traced, required }
            -- Single element map
            pure ((), elt)
        checkProducts (const NoRequestBody) check elements
      checkResponses = do
        let
          responses = (_operationResponses . operation) <$> prodCons
          respEnv = HCons (swapProdCons respDefs)
            $ HCons (swapProdCons headerDefs)
            $ HCons (swapProdCons schemaDefs) HNil
        localStep OperationResponsesStep
          $ swapRoles $ checkCompatibility respEnv responses
      checkCallbacks = (error "FIXME: not implemented")
      checkOperationSecurity = (error "FIXME: not implemented")
      checkServers = (error "FIXME: not implemented")
      bodyDefs = getH @(ProdCons (Definitions RequestBody)) env
      respDefs = getH @(ProdCons (Definitions Response)) env
      headerDefs =  getH @(ProdCons (Definitions Header)) env
      schemaDefs = getH @(ProdCons (Definitions Schema)) env

-- instance Subtree Operation where
--   type
--     CheckEnv Operation =
--       '[ ProdCons (Definitions Param)
--        , ProdCons (Definitions RequestBody)
--        , ProdCons (Definitions SecurityScheme)
--        , ProdCons (Definitions Response)
--        , ProdCons (Definitions Header)
--        , ProdCons (Definitions Schema)
--        ]
--   data CheckIssue Operation
--     = ParamNotMatched Text -- Param name
--     | NoRequestBody
--     | CallbacksNotSupported
--     | SecurityRequirementNotMet Int -- security indexs
--     | ServerNotConsumed Int -- server index
--     deriving (Eq, Ord, Show)
--   checkCompatibility env prodCons = do
--     let ProdCons {producer = pNonPathParams, consumer = cNonPathParams} = do
--           op <- _operationParameters <$> prodCons
--           defParams <- getH @(ProdCons (Definitions Param)) env
--           pure $
--             filter ((/= ParamPath) . _paramIn . getTraced)
--               . fmap (dereference defParams)
--               $ op
--         reqBody = do
--           op <- _operationRequestBody <$> prodCons
--           reqDefs <- getH @(ProdCons (Definitions RequestBody)) env
--           pure $ fmap (dereference reqDefs) op
--     for_ pNonPathParams $ \p@(Traced _ param) ->
--       anyOfAt
--         producer
--         (ParamNotMatched $ _paramName param)
--         [ checkProdCons env . fmap (retrace (step OperationParamsStep)) $ ProdCons p c
--         | c <- cNonPathParams
--         ]
--     case reqBody of
--       ProdCons Nothing Nothing -> pure ()
--       ProdCons (Just pBody) (Just cBody) ->
--         localStep OperationRequestBodyStep $
--           checkProdCons env (ProdCons pBody cBody)
--       ProdCons Nothing (Just _) -> issueAt producer NoRequestBody
--       ProdCons (Just _) Nothing -> issueAt consumer NoRequestBody
--     localStep OperationResponsesStep $
--       checkCompatibility env $ _operationResponses <$> prodCons
--     -- FIXME: https://github.com/typeable/openapi-diff/issues/27
--     case IOHM.null . _operationCallbacks <$> prodCons of
--       (ProdCons True True) -> pure ()
--       (ProdCons False _) -> issueAt producer CallbacksNotSupported
--       (ProdCons _ False) -> issueAt consumer CallbacksNotSupported
--     -- FIXME: https://github.com/typeable/openapi-diff/issues/28
--     sequenceA_
--       [ anyOfAt
--         producer
--         (SecurityRequirementNotMet i)
--         [ localStep OperationSecurityRequirementStep $
--           checkCompatibility env $ ProdCons prodSecurity consSecurity
--         | consSecurity <- _operationSecurity . consumer $ prodCons
--         ]
--       | (i, prodSecurity) <- zip [0 ..] . _operationSecurity . producer $ prodCons
--       ]
--     -- FIXME: https://github.com/typeable/openapi-diff/issues/29s
--     sequenceA_
--       [ anyOfAt
--         producer
--         (ServerNotConsumed i)
--         [ localStep OperationServerStep $
--           checkCompatibility env $ ProdCons pServer cServer
--         | cServer <- _operationServers . consumer $ prodCons
--         ]
--       | (i, pServer) <- zip [0 ..] . _operationServers . producer $ prodCons
--       ]
--     pure ()

instance Steppable MatchedOperation (Referenced Param) where
  data Step MatchedOperation (Referenced Param) = OperationParamsStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation (Referenced RequestBody) where
  data Step MatchedOperation (Referenced RequestBody) = OperationRequestBodyStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation Responses where
  data Step MatchedOperation Responses = OperationResponsesStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation SecurityRequirement where
  data Step MatchedOperation SecurityRequirement = OperationSecurityRequirementStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation Server where
  data Step MatchedOperation Server = OperationServerStep
    deriving (Eq, Ord, Show)
