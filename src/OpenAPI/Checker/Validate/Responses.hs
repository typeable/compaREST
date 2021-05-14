{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Header ()
import OpenAPI.Checker.Validate.Link ()
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()
import OpenAPI.Checker.Validate.Sums

tracedResponses :: Traced Responses -> IOHM.InsOrdHashMap HttpStatusCode (Traced (Referenced Response))
tracedResponses resp =
  IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseCodeStep k))) $
    _responsesResponses $ extract resp

instance Subtree Responses where
  type SubtreeLevel Responses = 'OperationLevel
  type
    CheckEnv Responses =
      '[ ProdCons (Traced (Definitions Response))
       , ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Link))
       ]

  checkStructuralCompatibility env pc = do
    structuralMaybe env $ _responsesDefault <$> pc
    iohmStructuralCompatibility env $ _responsesResponses <$> pc
    pure ()

  -- Roles are already swapped. Producer is a server and consumer is a
  -- client. Response codes are sum-like entity because we can answer with only
  -- one element
  checkSemanticCompatibility env beh prodCons = do
    let defs = getH @(ProdCons (Traced (Definitions Response))) env
        check code resps = checkCompatibility @Response env (beh >>> step (WithStatusCode code)) resps
        elements = getEls <$> defs <*> prodCons
        getEls respDef resps = M.fromList $ do
          (code, respRef) <- IOHM.toList $ tracedResponses resps
          pure (code, dereference respDef respRef)
    checkSums beh ResponseCodeNotFound check elements

tracedContent :: Traced Response -> IOHM.InsOrdHashMap MediaType (Traced MediaTypeObject)
tracedContent resp =
  IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseMediaObject k))) $
    _responseContent $ extract resp

tracedHeaders :: Traced Response -> IOHM.InsOrdHashMap HeaderName (Traced (Referenced Header))
tracedHeaders resp =
  IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseHeader k))) $
    _responseHeaders $ extract resp

instance Issuable 'ResponseLevel where
  data Issue 'ResponseLevel
    = ResponseMediaTypeMissing MediaType
    | ResponseHeaderMissing HeaderName
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Behavable 'ResponseLevel 'PayloadLevel where
  data Behave 'ResponseLevel 'PayloadLevel
    = ResponsePayload
    deriving stock (Eq, Ord, Show)

instance Behavable 'ResponseLevel 'HeaderLevel where
  data Behave 'ResponseLevel 'HeaderLevel
    = InHeader HeaderName
    deriving stock (Eq, Ord, Show)

instance Subtree Response where
  type SubtreeLevel Response = 'ResponseLevel
  type
    CheckEnv Response =
      '[ ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Link))
       ]
  checkStructuralCompatibility env pc = do
    iohmStructuralCompatibility env $ _responseContent <$> pc
    iohmStructuralCompatibility env $ _responseHeaders <$> pc
    iohmStructuralCompatibility env $ _responseLinks <$> pc
    pure ()
  checkSemanticCompatibility env beh prodCons = do
    -- Roles are already swapped. Producer is a server and consumer is a client
    checkMediaTypes
    checkHeaders
    pure ()
    where
      checkMediaTypes = do
        -- Media types are sum-like entity
        let check mediaType mtObj =
              let mtEnv = HCons mediaType $ env
               in checkCompatibility @MediaTypeObject mtEnv (beh >>> step ResponsePayload) mtObj
            elements = getEls <$> prodCons
            getEls resp = M.fromList . IOHM.toList $ tracedContent resp
        checkSums beh ResponseMediaTypeMissing check elements

      checkHeaders = do
        -- Headers are product-like entities
        let check name hdrs = checkCompatibility @Header env (beh >>> step (InHeader name)) hdrs
            elements = getEls <$> headerDefs <*> prodCons
            getEls headerDef resp = M.fromList $ do
              (hname, headerRef) <- IOHM.toList $ tracedHeaders resp
              let header = dereference headerDef headerRef
              pure
                ( hname
                , ProductLike
                    { productValue = header
                    , required = fromMaybe False . _headerRequired . extract $ header
                    }
                )
        checkProducts beh ResponseHeaderMissing check elements
      headerDefs = getH @(ProdCons (Traced (Definitions Header))) env

instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response) = ResponseCodeStep HttpStatusCode
    deriving stock (Eq, Ord, Show)

instance Steppable Response MediaTypeObject where
  data Step Response MediaTypeObject = ResponseMediaObject MediaType
    deriving stock (Eq, Ord, Show)

instance Steppable Response (Referenced Header) where
  data Step Response (Referenced Header) = ResponseHeader HeaderName
    deriving stock (Eq, Ord, Show)
