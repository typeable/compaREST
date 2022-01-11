{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.MediaTypeObject
  ( Issue (..),
    Behave (..),
  )
where

import Data.Foldable as F
import Data.Functor
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Header ()
import Data.OpenApi.Compare.Validate.Products
import Data.OpenApi.Compare.Validate.Schema ()
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Media (MediaType, mainType, subType)
import Text.Pandoc.Builder

tracedSchema :: Traced MediaTypeObject -> Maybe (Traced (Referenced Schema))
tracedSchema mto = _mediaTypeObjectSchema (extract mto) <&> traced (ask mto >>> step MediaTypeSchema)

-- FIXME: This should be done through 'MediaTypeEncodingMapping'
tracedEncoding :: Traced MediaTypeObject -> InsOrdHashMap Text (Traced Encoding)
tracedEncoding mto =
  IOHM.mapWithKey (\k -> traced (ask mto >>> step (MediaTypeParamEncoding k))) $
    _mediaTypeObjectEncoding $ extract mto

instance Issuable 'PayloadLevel where
  data Issue 'PayloadLevel
    = MediaTypeSchemaRequired
    | MediaEncodingMissing Text
    | EncodingNotSupported
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    EncodingNotSupported -> Unsupported
    _ -> CertainIssue

  describeIssue _ MediaTypeSchemaRequired = para "Media type expected, but was not specified."
  describeIssue Forward (MediaEncodingMissing enc) = para $ "Media encoding " <> str enc <> " has been removed."
  describeIssue Backward (MediaEncodingMissing enc) = para $ "Media encoding " <> str enc <> " added."
  describeIssue _ EncodingNotSupported = para "CompaREST does not currently support media encodings other than JSON."

instance Behavable 'PayloadLevel 'SchemaLevel where
  data Behave 'PayloadLevel 'SchemaLevel
    = PayloadSchema
    deriving stock (Eq, Ord, Show)
  describeBehavior PayloadSchema = "JSON Schema"

instance Subtree MediaTypeObject where
  type SubtreeLevel MediaTypeObject = 'PayloadLevel
  type
    CheckEnv MediaTypeObject =
      '[ MediaType
       , ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Header))
       ]
  checkStructuralCompatibility env pc = do
    structuralMaybe env $ tracedSchema <$> pc
    structuralEq $ fmap _mediaTypeObjectExample <$> pc
    iohmStructural env $ stepTraced MediaTypeEncodingMapping . fmap _mediaTypeObjectEncoding <$> pc
    pure ()
  checkSemanticCompatibility env beh prodCons@(ProdCons p c) = do
    if
        | "multipart" == mainType mediaType -> checkEncoding
        | "application" == mainType mediaType
            && "x-www-form-urlencoded" == subType mediaType ->
          checkEncoding
        | otherwise -> pure ()
    -- If consumer requires schema then producer must produce compatible
    -- request
    for_ (tracedSchema c) $ \consRef ->
      case tracedSchema p of
        Nothing -> issueAt beh MediaTypeSchemaRequired
        Just prodRef ->
          checkCompatibility (beh >>> step PayloadSchema) env $
            ProdCons prodRef consRef
    pure ()
    where
      mediaType = getH @MediaType env
      checkEncoding =
        let -- Parameters of the media type are product-like entities
            getEncoding mt =
              M.fromList $
                (IOHM.toList $ tracedEncoding mt) <&> \(k, enc) ->
                  ( k
                  , ProductLike
                      { productValue = enc
                      , required = True
                      }
                  )
            encProdCons = getEncoding <$> prodCons
         in checkProducts
              beh
              MediaEncodingMissing
              (const $ checkCompatibility beh env)
              encProdCons

instance Subtree Encoding where
  type SubtreeLevel Encoding = 'PayloadLevel
  type
    CheckEnv Encoding =
      '[ ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       ]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _encodingContentType <$> pc
    iohmStructural env $ stepTraced EncodingHeaderStep . fmap _encodingHeaders <$> pc
    structuralEq $ fmap _encodingStyle <$> pc
    structuralEq $ fmap _encodingExplode <$> pc
    structuralEq $ fmap _encodingAllowReserved <$> pc
    pure ()

  --  FIXME: Support only JSON body for now. Then Encoding is checked only for
  --  multipart/form-url-encoded
  --  #54
  checkSemanticCompatibility _env beh _pc =
    issueAt beh EncodingNotSupported

instance Steppable MediaTypeObject (Referenced Schema) where
  data Step MediaTypeObject (Referenced Schema) = MediaTypeSchema
    deriving stock (Eq, Ord, Show)

instance Steppable MediaTypeObject (Definitions Encoding) where
  data Step MediaTypeObject (Definitions Encoding) = MediaTypeEncodingMapping
    deriving stock (Eq, Ord, Show)

instance Steppable MediaTypeObject Encoding where
  data Step MediaTypeObject Encoding = MediaTypeParamEncoding Text
    deriving stock (Eq, Ord, Show)

instance Steppable Encoding (Definitions (Referenced Header)) where
  data Step Encoding (Definitions (Referenced Header)) = EncodingHeaderStep
    deriving stock (Eq, Ord, Show)

instance Behavable 'OperationLevel 'ResponseLevel where
  data Behave 'OperationLevel 'ResponseLevel
    = WithStatusCode HttpStatusCode
    deriving stock (Eq, Ord, Show)
  describeBehavior (WithStatusCode c) = "Response code " <> (fromString . show $ c)

instance Issuable 'OperationLevel where
  data Issue 'OperationLevel
    = ConsumerDoesntHaveResponseCode HttpStatusCode
    | ParamNotMatched Text
    | PathFragmentNotMatched Int
    | NoRequestBody
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    _ -> CertainIssue
  describeIssue Forward (ConsumerDoesntHaveResponseCode c) =
    para $ "Response code " <> (str . T.pack . show $ c) <> " has been removed."
  describeIssue Backward (ConsumerDoesntHaveResponseCode c) =
    para $ "Response code " <> (str . T.pack . show $ c) <> " has been added."
  describeIssue Forward (ParamNotMatched param) =
    para $ "Parameter " <> code param <> " has become required."
  describeIssue Backward (ParamNotMatched param) =
    para $ "Parameter " <> code param <> " is no longer required."
  describeIssue _ (PathFragmentNotMatched i) =
    -- TODO: Indices are meaningless in this context. Replace with a better error.
    para $ "Path fragment " <> (str . T.pack . show $ i) <> " not matched."
  describeIssue Forward NoRequestBody = para "Request body has been added."
  describeIssue Backward NoRequestBody = para "Request body has been removed."
