{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Data.Foldable
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject ()
import OpenAPI.Checker.Validate.Schema ()

instance Subtree Responses where
  type CheckEnv Responses =
    '[ ProdCons (Definitions Response)
     , ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Responses = ResponseCodeNotFound HttpStatusCode
    deriving (Eq, Ord, Show)
  -- Here we are checking responses, so, the consumer and producer swap their
  -- roles. The consumer now generates the response and producer consumes
  -- it. So, the logic is swapped.
  checkCompatibility env (ProdCons p c) = do
    let defs = getH @(ProdCons (Definitions Response)) env
    for_ (IOHM.toList $  _responsesResponses p) $ \ (prodStatus, prodRef) ->
      case IOHM.lookup prodStatus $ _responsesResponses c of
        Nothing -> issueAt consumer $ ResponseCodeNotFound prodStatus
        Just consRef -> do
          let tracedRefs = dereference <$> defs <*> ProdCons prodRef consRef
          localStep (ResponseCodeStep prodStatus)
            $ checkProdCons env tracedRefs
    --  FIXME: Do we need to check "default" fields somehow here?

instance Subtree Response where
  type CheckEnv Response =
    '[ ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Response
    = ResponseMediaTypeMissing MediaType
    | ResponseHeaderMissing HeaderName
    deriving (Eq, Ord, Show)
  -- Here we are checking responses, so, the consumer and producer swap their
  -- roles. The consumer now generates the response and producer consumes
  -- it. So, the logic is swapped.
  checkCompatibility env (ProdCons p c) = do
    checkMediaTypes
    checkHeaders
    pure ()
    where
      -- Each response type expected by producer must be in the consumer (logic
      -- is swapped)
      checkMediaTypes = do
        for_ (IOHM.toList $ _responseContent p) $ \ (mediaType, prodMediaObject) ->
          case IOHM.lookup mediaType $ _responseContent c of
            Nothing -> issueAt consumer $ ResponseMediaTypeMissing mediaType
            Just consMediaObject -> localStep (ResponseMediaObject mediaType)
              $ swapRoles
              $ checkCompatibility @MediaTypeObject (HCons mediaType env)
              $ ProdCons consMediaObject prodMediaObject
      checkHeaders = do
        for_ (IOHM.toList $ _responseHeaders p) $ \ (hname, prodRef) ->
          case IOHM.lookup hname $ _responseHeaders c of
            Nothing -> issueAt consumer $ ResponseHeaderMissing hname
            Just consRef -> do
              let headerRefs = dereference <$> headerDefs <*> ProdCons prodRef consRef
              localStep (ResponseHeader hname)
                $ swapRoles
                $ checkProdCons env $ swapProdCons headerRefs
      headerDefs = getH @(ProdCons (Definitions Header)) env

instance Subtree Header where
  type CheckEnv Header = '[ProdCons (Definitions Schema)]
  data CheckIssue Header
    = RequiredHeaderMissing
    | NonEmptyHeaderRequired
    | HeaderSchemaRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    if (fromMaybe False $ _headerRequired c) && not (fromMaybe False $ _headerRequired p)
      then issueAt producer RequiredHeaderMissing else pure ()
    if not (fromMaybe False $ _headerAllowEmptyValue c) && (fromMaybe False $ _headerAllowEmptyValue p)
      then issueAt producer NonEmptyHeaderRequired else pure ()
    for_ (_headerSchema c) $ \consRef ->
      case (_headerSchema p) of
        Nothing -> issueAt producer HeaderSchemaRequired
        Just prodRef -> do
          localStep HeaderSchema
            $ checkCompatibility env $ ProdCons prodRef consRef
    pure ()

instance Steppable Header (Referenced Schema) where
  data Step Header (Referenced Schema) = HeaderSchema
    deriving (Eq, Ord, Show)

instance Steppable Response (Referenced Header) where
  data Step Response (Referenced Header) = ResponseHeader HeaderName
    deriving (Eq, Ord, Show)

instance Steppable Response MediaTypeObject where
  data Step Response MediaTypeObject = ResponseMediaObject MediaType
    deriving (Eq, Ord, Show)

instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response) = ResponseCodeStep HttpStatusCode
    deriving (Eq, Ord, Show)
