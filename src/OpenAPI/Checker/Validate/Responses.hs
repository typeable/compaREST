{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Control.Lens
import Data.Foldable
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()
import OpenAPI.Checker.Validate.Sums

tracedResponses :: Traced r Responses -> IOHM.InsOrdHashMap HttpStatusCode (Traced r (Referenced Response))
tracedResponses resp = IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseCodeStep k)))
  $ _responsesResponses $ extract resp

instance Subtree Responses where
  type CheckEnv Responses =
    '[ ProdCons (Definitions Response)
     , ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Responses
    deriving (Eq, Ord, Show)
  -- Roles are already swapped. Producer is a server and consumer is a
  -- client. Response codes are sum-like entity because we can answer with only
  -- one element
  checkCompatibility env prodCons = do
    let
      defs = getH @(ProdCons (Definitions Response)) env
      check resps = checkCompatibility @Response env resps
      elements = getEls <$> defs <*> prodCons
      getEls respDef resps = M.fromList $ do
        (code, respRef) <- IOHM.toList $ tracedResponses resps
        pure (code, dereference respDef respRef)
    checkSums (const ResponseCodeNotFound) (const check) elements

tracedContent :: Traced r Response -> IOHM.InsOrdHashMap MediaType (Traced r MediaTypeObject)
tracedContent resp = IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseMediaObject k)))
  $ _responseContent $ extract resp

tracedHeaders :: Traced r Response -> IOHM.InsOrdHashMap HeaderName (Traced r (Referenced Header))
tracedHeaders resp = IOHM.mapWithKey (\k -> traced (ask resp >>> step (ResponseHeader k)))
  $ _responseHeaders $ extract resp

instance Subtree Response where
  type CheckEnv Response =
    '[ ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Response
    = ResponseCodeNotFound
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons = do
    -- Roles are already swapped. Producer is a server and consumer is a client
    checkMediaTypes
    checkHeaders
    pure ()
    where
      checkMediaTypes = do
        -- Media types are sum-like entity
        let
          check mediaType mtObj =
            let mtEnv = HCons mediaType $ env
            in checkCompatibility @MediaTypeObject mtEnv mtObj
          elements = getEls <$> prodCons
          getEls resp = M.fromList . IOHM.toList $ tracedContent resp
        checkSums (const ResponseMediaTypeMissing) check elements

      checkHeaders = do
        -- Headers are product-like entities
        let
          check hdrs = checkCompatibility @Header env hdrs
          elements = getEls <$> headerDefs <*> prodCons
          getEls headerDef resp = M.fromList $ do
            (hname, headerRef) <- IOHM.toList $ tracedHeaders resp
            let header = dereference headerDef headerRef
            pure (hname, ProductLike
              { tracedValue = header
              , required = fromMaybe False . _headerRequired . extract $ header
              })
        checkProducts (const ResponseHeaderMissing) (const check) elements
      headerDefs = getH @(ProdCons (Definitions Header)) env

tracedSchema :: Traced r Header -> Maybe (Traced r (Referenced Schema))
tracedSchema hdr = _headerSchema (extract hdr) <&> traced (ask hdr >>> step HeaderSchema)

instance Subtree Header where
  type CheckEnv Header = '[ProdCons (Definitions Schema)]
  data CheckIssue Header
    = ResponseHeaderMissing
    | RequiredHeaderMissing
    | NonEmptyHeaderRequired
    | HeaderSchemaRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    if (fromMaybe False $ _headerRequired $ extract c) && not (fromMaybe False $ _headerRequired $ extract p)
      then issueAt p RequiredHeaderMissing else pure ()
    if not (fromMaybe False $ _headerAllowEmptyValue $ extract c) && (fromMaybe False $ _headerAllowEmptyValue $ extract p)
      then issueAt p NonEmptyHeaderRequired else pure ()
    for_ (tracedSchema c) $ \consRef ->
      case tracedSchema p of
        Nothing -> issueAt p HeaderSchemaRequired
        Just prodRef -> checkCompatibility env (ProdCons prodRef consRef)
    pure ()

instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response) = ResponseCodeStep HttpStatusCode
    deriving (Eq, Ord, Show)

instance Steppable Header (Referenced Schema) where
  data Step Header (Referenced Schema) = HeaderSchema
    deriving (Eq, Ord, Show)

instance Steppable Response (Referenced Header) where
  data Step Response (Referenced Header) = ResponseHeader HeaderName
    deriving (Eq, Ord, Show)

instance Steppable Response MediaTypeObject where
  data Step Response MediaTypeObject = ResponseMediaObject MediaType
    deriving (Eq, Ord, Show)
