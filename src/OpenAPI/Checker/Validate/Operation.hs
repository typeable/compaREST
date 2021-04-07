{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Operation (Step (..)) where

import Data.Foldable
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import Data.Text (Text)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Param ()
import OpenAPI.Checker.Validate.RequestBody ()
import OpenAPI.Checker.Validate.Responses ()
import OpenAPI.Checker.Validate.SecurityRequirement ()
import OpenAPI.Checker.Validate.Server ()

instance Subtree Operation where
  type
    CheckEnv Operation =
      '[ ProdCons (Definitions Param)
       , ProdCons (Definitions RequestBody)
       , ProdCons (Definitions SecurityScheme)
       ]
  data CheckIssue Operation
    = ParamNotMatched Text -- Param name
    | NoRequestBody
    | CallbacksNotSupported
    | SecurityRequirementNotMet Int -- security indexs
    | ServerNotConsumed Int -- server index
    deriving (Eq, Ord, Show)
  normalizeTrace = undefined
  checkCompatibility env prodCons = do
    let ProdCons {producer = pNonPathParams, consumer = cNonPathParams} = do
          op <- _operationParameters <$> prodCons
          defParams <- getH @(ProdCons (Definitions Param)) env
          pure $
            filter ((/= ParamPath) . _paramIn . getTraced)
              . fmap (dereference defParams)
              $ op
        reqBody = do
          op <- _operationRequestBody <$> prodCons
          reqDefs <- getH @(ProdCons (Definitions RequestBody)) env
          pure $ fmap (dereference reqDefs) op
    for_ pNonPathParams $ \p@(Traced _ param) ->
      anyOfAt
        producer
        (ParamNotMatched $ _paramName param)
        [ checkProdCons HNil . fmap (retrace (step OperationParamsStep)) $ ProdCons p c
        | c <- cNonPathParams
        ]
    case reqBody of
      ProdCons Nothing Nothing -> pure ()
      ProdCons (Just pBody) (Just cBody) ->
        localStep OperationRequestBodyStep $
          checkProdCons HNil (ProdCons pBody cBody)
      ProdCons Nothing (Just _) -> issueAt producer NoRequestBody
      ProdCons (Just _) Nothing -> issueAt consumer NoRequestBody
    localStep OperationResponsesStep $
      checkCompatibility HNil $ _operationResponses <$> prodCons
    -- FIXME: https://github.com/typeable/openapi-diff/issues/27
    case IOHM.null . _operationCallbacks <$> prodCons of
      (ProdCons True True) -> pure ()
      (ProdCons False _) -> issueAt producer CallbacksNotSupported
      (ProdCons _ False) -> issueAt consumer CallbacksNotSupported
    -- FIXME: https://github.com/typeable/openapi-diff/issues/28
    sequenceA_
      [ anyOfAt
        producer
        (SecurityRequirementNotMet i)
        [ localStep OperationSecurityRequirementStep $
          checkCompatibility env $ ProdCons prodSecurity consSecurity
        | consSecurity <- _operationSecurity . consumer $ prodCons
        ]
      | (i, prodSecurity) <- zip [0 ..] . _operationSecurity . producer $ prodCons
      ]
    -- FIXME: https://github.com/typeable/openapi-diff/issues/29s
    sequenceA_
      [ anyOfAt
        producer
        (ServerNotConsumed i)
        [ localStep OperationServerStep $
          checkCompatibility env $ ProdCons pServer cServer
        | cServer <- _operationServers . consumer $ prodCons
        ]
      | (i, pServer) <- zip [0 ..] . _operationServers . producer $ prodCons
      ]
    pure ()

instance Steppable PathItem Operation where
  data Step PathItem Operation
    = GetStep
    | PutStep
    | PostStep
    | DeleteStep
    | OptionsStep
    | HeadStep
    | PatchStep
    | TraceStep
    deriving (Eq, Ord, Show)

instance Steppable Operation (Referenced Param) where
  data Step Operation (Referenced Param) = OperationParamsStep
    deriving (Eq, Ord, Show)

instance Steppable Operation (Referenced RequestBody) where
  data Step Operation (Referenced RequestBody) = OperationRequestBodyStep
    deriving (Eq, Ord, Show)

instance Steppable Operation Responses where
  data Step Operation Responses = OperationResponsesStep
    deriving (Eq, Ord, Show)

instance Steppable Operation SecurityRequirement where
  data Step Operation SecurityRequirement = OperationSecurityRequirementStep
    deriving (Eq, Ord, Show)

instance Steppable Operation Server where
  data Step Operation Server = OperationServerStep
    deriving (Eq, Ord, Show)
