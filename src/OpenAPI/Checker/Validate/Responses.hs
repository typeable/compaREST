{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Data.Foldable
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace

instance Subtree Responses where
  type CheckEnv Responses = '[ProdCons (Definitions Response)]
  data CheckIssue Responses = ResponseCodeNotFound
    deriving (Eq, Ord, Show)
  -- Here we are checking responses, so, the consumer and producer swap their
  -- roles. The consumer now generates the response and producer consumes
  -- it. So, the logic is swapped.
  checkCompatibility env (ProdCons p c) = do
    let defs = getH @(ProdCons (Definitions Response)) env
    for_ (IOHM.toList $  _responsesResponses p) $ \ (prodStatus, prodRef) ->
      case IOHM.lookup prodStatus $ _responsesResponses c of
        Nothing -> issueAt consumer ResponseCodeNotFound
        Just consRef -> do
          let tracedRefs = dereferenceTraced <$> defs <*> ProdCons prodRef consRef
          localStep (ResponseCodeStep prodStatus)
            $ checkProdCons HNil tracedRefs
    --  FIXME: Do we need to check "default" fields somehow here?

instance Subtree Response where
  type CheckEnv Response = '[ ]
  data CheckIssue Response = ResponseMediaTypeMissing
    deriving (Eq, Ord, Show)
  -- Here we are checking responses, so, the consumer and producer swap their
  -- roles. The consumer now generates the response and producer consumes
  -- it. So, the logic is swapped.
  checkCompatibility _ (ProdCons p c) = do
    -- Each response type expected by producer must be in the consumer (logic is
    -- swapped)
    for_ (IOHM.toList $ _responseContent p) $ \ (mediaType, prodMediaObject) ->
      case IOHM.lookup mediaType $ _responseContent c of
        Nothing -> issueAt consumer ResponseMediaTypeMissing
        Just consMediaType ->
          (error "FIXME: How to call `checkCompatibility @MediaTypeObject` here?")
          prodMediaObject consMediaType
          -- We have reversed logic for response, so the media object from
          -- producer must be considered as from consumer and vise versa. We
          -- could @checkCompatibility _ (ProdCons consMediaType prodMediaType)@
          -- But we have a context in CompatM


instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response) = ResponseCodeStep HttpStatusCode
    deriving (Eq, Ord, Show)
