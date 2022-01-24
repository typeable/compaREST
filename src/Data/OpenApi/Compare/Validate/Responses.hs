{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.OpenApi.Compare.Validate.Responses
  ( Behave (..),
  )
where

import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.References
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Header ()
import Data.OpenApi.Compare.Validate.Link ()
import Data.OpenApi.Compare.Validate.MediaTypeObject
import Data.OpenApi.Compare.Validate.Products
import Data.OpenApi.Compare.Validate.Schema ()
import Data.OpenApi.Compare.Validate.Sums
import qualified Data.Text as T
import Network.HTTP.Media (MediaType)
import Text.Pandoc.Builder

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
    structuralMaybe env $ sequence . stepTraced ResponseDefaultStep . fmap _responsesDefault <$> pc
    iohmStructural env $ stepTraced ResponsesStep . fmap _responsesResponses <$> pc
    pure ()

  -- Roles are already swapped. Producer is a server and consumer is a
  -- client. Response codes are sum-like entity because we can answer with only
  -- one element
  checkSemanticCompatibility env beh prodCons = do
    let defs = getH @(ProdCons (Traced (Definitions Response))) env
        check code resps = checkCompatibility @Response (beh >>> step (WithStatusCode code)) env resps
        elements = getEls <$> defs <*> prodCons
        getEls respDef resps = M.fromList $ do
          (code, respRef) <- IOHM.toList $ tracedResponses resps
          pure (code, dereference respDef respRef)
    checkSums beh ConsumerDoesntHaveResponseCode check elements

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
    = ConsumerDoesntHaveMediaType MediaType
    | ProducerDoesntHaveResponseHeader HeaderName
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    _ -> CertainIssue
  describeIssue Forward (ConsumerDoesntHaveMediaType t) =
    para $ "Media type was removed: " <> (code . T.pack . show $ t) <> "."
  describeIssue Backward (ConsumerDoesntHaveMediaType t) =
    para $ "Media type was added: " <> (code . T.pack . show $ t) <> "."
  describeIssue Forward (ProducerDoesntHaveResponseHeader h) =
    para $ "New header was added " <> code h <> "."
  describeIssue Backward (ProducerDoesntHaveResponseHeader h) =
    para $ "Header was removed " <> code h <> "."

instance Behavable 'ResponseLevel 'PayloadLevel where
  data Behave 'ResponseLevel 'PayloadLevel
    = ResponsePayload
    deriving stock (Eq, Ord, Show)

  describeBehavior ResponsePayload = "Payload"

instance Behavable 'ResponseLevel 'HeaderLevel where
  data Behave 'ResponseLevel 'HeaderLevel
    = InHeader HeaderName
    deriving stock (Eq, Ord, Show)

  describeBehavior (InHeader name) = "Header " <> code name

instance Subtree Response where
  type SubtreeLevel Response = 'ResponseLevel
  type
    CheckEnv Response =
      '[ ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Link))
       ]
  checkStructuralCompatibility env pc = do
    iohmStructural env $ stepTraced ResponseMediaObjects . fmap _responseContent <$> pc
    iohmStructural env $ stepTraced ResponseHeaders . fmap _responseHeaders <$> pc
    iohmStructural env $ stepTraced ResponseLinks . fmap _responseLinks <$> pc
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
              let mtEnv = HCons mediaType env
               in checkCompatibility @MediaTypeObject (beh >>> step ResponsePayload) mtEnv mtObj
            elements = getEls <$> prodCons
            getEls resp = M.fromList . IOHM.toList $ tracedContent resp
        checkSums beh ConsumerDoesntHaveMediaType check elements

      checkHeaders = do
        -- Headers are product-like entities
        let check name hdrs = checkCompatibility @Header (beh >>> step (InHeader name)) env hdrs
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
        checkProducts beh ProducerDoesntHaveResponseHeader check elements
      headerDefs = getH @(ProdCons (Traced (Definitions Header))) env

instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response)
    = ResponseCodeStep HttpStatusCode
    | ResponseDefaultStep
    deriving stock (Eq, Ord, Show)

instance Steppable Response MediaTypeObject where
  data Step Response MediaTypeObject = ResponseMediaObject MediaType
    deriving stock (Eq, Ord, Show)

instance Steppable Response (IOHM.InsOrdHashMap MediaType MediaTypeObject) where
  data Step Response (IOHM.InsOrdHashMap MediaType MediaTypeObject) = ResponseMediaObjects
    deriving stock (Eq, Ord, Show)

instance Steppable Response (Definitions (Referenced Header)) where
  data Step Response (Definitions (Referenced Header)) = ResponseHeaders
    deriving stock (Eq, Ord, Show)

instance Steppable Response (Definitions (Referenced Link)) where
  data Step Response (Definitions (Referenced Link)) = ResponseLinks
    deriving stock (Eq, Ord, Show)

instance Steppable Response (Referenced Header) where
  data Step Response (Referenced Header) = ResponseHeader HeaderName
    deriving stock (Eq, Ord, Show)

instance Steppable Responses (IOHM.InsOrdHashMap HttpStatusCode (Referenced Response)) where
  data Step Responses (IOHM.InsOrdHashMap HttpStatusCode (Referenced Response)) = ResponsesStep
    deriving stock (Eq, Ord, Show)
