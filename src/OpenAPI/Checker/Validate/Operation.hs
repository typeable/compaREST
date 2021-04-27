{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Operation
  ( MatchedOperation(..)
  ) where


import Data.Foldable as F
import Data.Functor
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
  { operation :: !Operation
  , pathParams :: ![Traced OpenApi Param]
  -- ^ Params from the PathItem
  , getPathFragments :: !([Traced OpenApi Param] -> [Traced OpenApi PathFragmentParam])
  -- ^ Path fragments traced from PathItem. Takes full list of
  -- operation-specific parameters
  }

type ParamKey = (ParamLocation, Text)

paramKey :: Param -> ParamKey
paramKey param = (_paramIn param, _paramName param)

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
    = ParamNotMatched ParamLocation Text
    -- ^ Non-path param has no pair
    | PathFragmentNotMatched Int
    -- ^ Path fragment with given position has no match
    | NoRequestBody
    | CallbacksNotSupported
    | SecurityRequirementNotMet Int -- security indexs
    | ServerNotConsumed Int -- server index
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons = withTrace $ \myTrace -> do
    checkParameters myTrace
    checkRequestBodies
    checkResponses
    checkCallbacks
    checkOperationSecurity
    checkServers
    pure ()
    where
      checkParameters myTrace = do
        let
          -- Merged parameters got from Operation and PathItem in one
          -- place. First element is path params, second is non-path params
          tracedParams :: ProdCons ([Traced OpenApi Param], [Traced OpenApi Param])
          tracedParams = getParams <$> myTrace <*> paramDefs <*> prodCons
          getParams root defs mp =
            let
              operationParamsMap :: Map ParamKey (Traced OpenApi Param)
              operationParamsMap = M.fromList $ do
                paramRef <- _operationParameters $ operation mp
                let
                  tracedParam = retrace root
                    $ dereferenceTraced defs
                    $ Traced (step $ OperationParamsStep) paramRef
                  key = paramKey $ getTraced tracedParam
                pure (key, tracedParam)
              pathParamsMap :: Map ParamKey (Traced OpenApi Param)
              pathParamsMap = M.fromList $ do
                param <- pathParams mp
                pure (paramKey $ getTraced param, param)
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
      checkPathParams pathParams = do
        let
          fragments :: ProdCons [Traced OpenApi PathFragmentParam]
          fragments = getFragments <$> pathParams <*> prodCons
          getFragments params mop = (getPathFragments mop) params
          -- Feed path parameters to the fragments getter
          check _ frags = checkCompatibility @PathFragmentParam env frags
          elements = fragments <&> \frags -> M.fromList $ zip [0..] $ do
            frag <- frags
            pure $ ProductLike
              { traced = frag
              , required = True }
        checkProducts' PathFragmentNotMatched check elements
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
      paramDefs = getH @(ProdCons (Definitions Param)) env

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
