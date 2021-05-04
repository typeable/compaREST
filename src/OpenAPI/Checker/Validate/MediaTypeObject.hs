{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.MediaTypeObject
  ( Issue(..)
  , Behave(..)
  ) where

import Control.Lens
import Data.Foldable as F
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.OpenApi
import Data.Text (Text)
import Network.HTTP.Media (MediaType, mainType, subType)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()

tracedSchema :: Traced MediaTypeObject -> Maybe (Traced (Referenced Schema))
tracedSchema mto = _mediaTypeObjectSchema (extract mto) <&> traced (ask mto >>> step MediaTypeSchema)

tracedEncoding :: Traced MediaTypeObject -> InsOrdHashMap Text (Traced Encoding)
tracedEncoding mto = IOHM.mapWithKey (\k -> traced (ask mto >>> step (MediaTypeParamEncoding k)))
  $ _mediaTypeObjectEncoding $ extract mto

instance Issuable 'PayloadLevel where
  data Issue 'PayloadLevel
    = PayloadMediaTypeNotFound
    | MediaEncodingIncompat
    | MediaTypeSchemaRequired
    | MediaEncodingMissing Text
    | EncodingNotSupported
    deriving (Eq, Ord, Show)
  issueIsUnsupported = \case
    EncodingNotSupported -> True
    _ -> False

instance Behavable 'PayloadLevel 'SchemaLevel where
  data Behave 'PayloadLevel 'SchemaLevel
    = PayloadSchema
    deriving (Eq, Ord, Show)

instance Subtree MediaTypeObject where
  type ToBehavior MediaTypeObject = 'PayloadLevel
  type CheckEnv MediaTypeObject =
    '[ MediaType
     , ProdCons (Traced (Definitions Schema))
     ]
  checkCompatibility env beh prodCons@(ProdCons p c) = do
    if | "multipart" == mainType mediaType -> checkEncoding
       | "application" == mainType mediaType &&
         "x-www-form-urlencoded" == subType mediaType -> checkEncoding
       | otherwise -> pure ()
    -- If consumer requires schema then producer must produce compatible
    -- request
    for_ (tracedSchema c) $ \consRef ->
        case tracedSchema p of
          Nothing -> issueAt beh MediaTypeSchemaRequired
          Just prodRef -> checkCompatibility env (beh >>> step PayloadSchema)
            $ ProdCons prodRef consRef
    pure ()
    where
      mediaType = getH @MediaType env
      checkEncoding =
        let
          -- Parameters of the media type are product-like entities
          getEncoding mt = M.fromList
            $ (IOHM.toList $ tracedEncoding mt) <&> \(k, enc) ->
            ( k
            , ProductLike
              { productValue = enc
              , required = True } )
          encProdCons = getEncoding <$> prodCons
        in checkProducts beh MediaEncodingMissing
           (const $ checkCompatibility HNil beh) encProdCons

instance Subtree Encoding where
  type ToBehavior Encoding = 'PayloadLevel
  type CheckEnv Encoding = '[]
    --  FIXME: Support only JSON body for now. Then Encoding is checked only for
    --  multipart/form-url-encoded
  checkCompatibility _env beh _pc =
    issueAt beh EncodingNotSupported

instance Steppable MediaTypeObject (Referenced Schema) where
  data Step MediaTypeObject (Referenced Schema) = MediaTypeSchema
    deriving (Eq, Ord, Show)

instance Steppable MediaTypeObject Encoding where
  data Step MediaTypeObject Encoding = MediaTypeParamEncoding Text
    deriving (Eq, Ord, Show)

instance Behavable 'OperationLevel 'ResponseLevel where
  data Behave 'OperationLevel 'ResponseLevel
    = WithStatusCode HttpStatusCode
    deriving stock (Eq, Ord, Show)

instance Issuable 'OperationLevel where
  data Issue 'OperationLevel
    = ResponseCodeNotFound HttpStatusCode
    | CallbacksNotSupported
    | ParamNotMatched Text
    | PathFragmentNotMatched Int
    | NoRequestBody
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported = \case
    CallbacksNotSupported -> True
    _ -> False
