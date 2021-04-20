{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module OpenAPI.Checker.Validate.Operation
  ( MatchedOperation (..)
  , CheckIssue (..)
  ) where


import Data.Foldable as F
import Data.Functor
import Data.HList
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Param
import OpenAPI.Checker.Validate.PathFragment
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.RequestBody
import OpenAPI.Checker.Validate.Responses ()

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

tracedParameters :: Traced r MatchedOperation -> [Traced r (Referenced Param)]
tracedParameters oper =
  [ traced (ask oper >>> step (OperationParamsStep i)) x
  | (i, x) <- zip [0..] $ _operationParameters . operation $ extract oper
  ]

tracedRequestBody :: Traced r MatchedOperation -> Maybe (Traced r (Referenced RequestBody))
tracedRequestBody oper = _operationRequestBody (operation $ extract oper) <&> traced (ask oper >>> step OperationRequestBodyStep)

tracedResponses :: Traced r MatchedOperation -> Traced r Responses
tracedResponses oper = traced (ask oper >>> step OperationResponsesStep)
  $ _operationResponses . operation $ extract oper

tracedSecurity :: Traced r MatchedOperation -> [Traced r SecurityRequirement]
tracedSecurity oper =
  [ traced (ask oper >>> step (OperationSecurityRequirementStep i)) x
  | (i, x) <- zip [0..] $ _operationSecurity . operation $ extract oper
  ]

tracedServers :: Traced r MatchedOperation -> [Traced r Server]
tracedServers oper =
  [ traced (ask oper >>> step (OperationServerStep i)) x
  | (i, x) <- zip [0..] $ _operationServers . operation $ extract oper
  ]

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
    = OperationMissing
    | CallbacksNotSupported
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons = do
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
          tracedParams = getParams <$> paramDefs <*> prodCons
          getParams defs mp =
            let
              operationParamsMap :: Map ParamKey (Traced OpenApi Param)
              operationParamsMap = M.fromList $ do
                paramRef <- tracedParameters mp
                let
                  param = dereference defs paramRef
                  key = paramKey . extract $ param
                pure (key, param)
              pathParamsMap :: Map ParamKey (Traced OpenApi Param)
              pathParamsMap = M.fromList $ do
                param <- pathParams . extract $ mp
                pure (paramKey . extract $ param, param)
              params = M.elems $ M.union operationParamsMap pathParamsMap
              -- We prefer params from Operation
              splitted = L.partition
                (\p -> (_paramIn . extract $ p) == ParamPath) params
            in splitted
        checkNonPathParams $ snd <$> tracedParams
        checkPathParams $ fst <$> tracedParams
        pure ()
      checkNonPathParams :: ProdCons [Traced OpenApi Param] -> CompatFormula ()
      checkNonPathParams params = do
        let
          elements = getEls <$> params
          getEls params = M.fromList $ do
            p <- params
            let
              k = (_paramIn . extract $ p, _paramName . extract $ p)
              v = ProductLike
                { tracedValue = p
                , required = fromMaybe False . _paramRequired . extract $ p
                }
            pure (k, v)
          check param = do
            checkCompatibility @Param (singletonH schemaDefs) param
        checkProducts (ParamNotMatched . snd) (const check) elements
      checkPathParams :: ProdCons [Traced OpenApi Param] -> CompatFormula ()
      checkPathParams pathParams = do
        let
          fragments :: ProdCons [Traced OpenApi PathFragmentParam]
          fragments = getFragments <$> pathParams <*> prodCons
          getFragments params mop = getPathFragments (extract mop) params
          -- Feed path parameters to the fragments getter
          check frags = checkCompatibility @PathFragmentParam env frags
          elements = fragments <&> \frags -> M.fromList $ zip [0 :: Int ..] $ do
            frag <- frags
            pure $ ProductLike
              { tracedValue = frag
              , required = True }
        checkProducts (const PathFragmentNotMatched) (const check) elements
      checkRequestBodies = do
        let
          check reqBody = checkCompatibility @RequestBody env reqBody
          elements = getReqBody <$> bodyDefs <*> prodCons
          getReqBody bodyDef mop = M.fromList $ do
            bodyRef <- F.toList . tracedRequestBody $ mop
            let
              body = dereference bodyDef bodyRef
            -- Single element map
            pure ((), ProductLike
              { tracedValue = body
              , required = fromMaybe False . _requestBodyRequired . extract $ body
              })
        checkProducts (const NoRequestBody) (const check) elements
      checkResponses = do
        let
          respEnv = HCons (swapProdCons respDefs)
            $ HCons (swapProdCons headerDefs)
            $ HCons (swapProdCons schemaDefs) HNil
          resps = tracedResponses <$> prodCons
        checkCompatibility respEnv $ swapProdCons resps
      checkCallbacks = pure () -- (error "FIXME: not implemented")
      checkOperationSecurity = pure () -- (error "FIXME: not implemented")
      checkServers = pure () -- (error "FIXME: not implemented")
      bodyDefs = getH @(ProdCons (Definitions RequestBody)) env
      respDefs = getH @(ProdCons (Definitions Response)) env
      headerDefs =  getH @(ProdCons (Definitions Header)) env
      schemaDefs = getH @(ProdCons (Definitions Schema)) env
      paramDefs = getH @(ProdCons (Definitions Param)) env

instance Steppable MatchedOperation (Referenced Param) where
  data Step MatchedOperation (Referenced Param) = OperationParamsStep Int
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation (Referenced RequestBody) where
  data Step MatchedOperation (Referenced RequestBody) = OperationRequestBodyStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation Responses where
  data Step MatchedOperation Responses = OperationResponsesStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation SecurityRequirement where
  data Step MatchedOperation SecurityRequirement = OperationSecurityRequirementStep Int
    deriving (Eq, Ord, Show)

instance Steppable MatchedOperation Server where
  data Step MatchedOperation Server = OperationServerStep Int
    deriving (Eq, Ord, Show)
