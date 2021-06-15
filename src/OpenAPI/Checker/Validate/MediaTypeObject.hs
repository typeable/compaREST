{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.MediaTypeObject
  ( Issue (..)
  , Behave (..)
  )
where

import Data.Foldable as F
import Data.Functor
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Media (MediaType, mainType, subType)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Header ()
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()
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
  issueIsUnsupported = \case
    EncodingNotSupported -> True
    _ -> False

  describeIssue MediaTypeSchemaRequired = para "Media type expected, but was not specified."
  describeIssue (MediaEncodingMissing enc) = para $ "Media encoding " <> str enc <> " expected, but was not specified."
  describeIssue EncodingNotSupported = para "OpenApi Diff does not currently support media encodings other than JSON."

instance Behavable 'PayloadLevel 'SchemaLevel where
  data Behave 'PayloadLevel 'SchemaLevel
    = PayloadSchema
    deriving stock (Eq, Ord, Show)
  describeBehaviour PayloadSchema = "JSON Schema"

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
          checkCompatibility env (beh >>> step PayloadSchema) $
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
              (const $ checkCompatibility env beh)
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
  --  https://github.com/typeable/openapi-diff/issues/54
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
  describeBehaviour (WithStatusCode c) = "Response code " <> (fromString . show $ c)

instance Issuable 'OperationLevel where
  data Issue 'OperationLevel
    = ResponseCodeNotFound HttpStatusCode
    | ParamNotMatched Text
    | PathFragmentNotMatched Int
    | NoRequestBody
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported = \case
    _ -> False
  describeIssue (ResponseCodeNotFound c) =
    para $ "Reponse code " <> (str . T.pack . show $ c) <> " is not supported."
  describeIssue (ParamNotMatched param) =
    para $ "Didn't expect parameter " <> code param <> " to be required, but it was."
  describeIssue (PathFragmentNotMatched i) =
    -- TODO: Indices are meaningless in this context. Replace with a better error.
    para $ "Path fragment " <> (str . T.pack . show $ i) <> " not matched."
  describeIssue NoRequestBody = para "Request body not specified."
