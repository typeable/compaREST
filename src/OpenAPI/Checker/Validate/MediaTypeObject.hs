{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.MediaTypeObject () where

import Data.Foldable as F
import Data.Functor
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.OpenApi
import Data.Text (Text)
import Network.HTTP.Media (MediaType, mainType, subType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()

instance Subtree MediaTypeObject where
  type CheckEnv MediaTypeObject =
    '[ MediaType
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue MediaTypeObject
    = MediaEncodingMissing Text
    | MediaEncodingIncompat
    | MediaTypeSchemaRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons@(ProdCons p c) = do
    if | "multipart" == mainType mediaType -> checkEncoding
       | "application" == mainType mediaType &&
         "x-www-form-urlencoded" == subType mediaType -> checkEncoding
       | otherwise -> pure ()
    -- If consumer requires schema then producer must produce compatible
    -- request
    for_ (_mediaTypeObjectSchema c) $ \consRef ->
        case _mediaTypeObjectSchema p of
          Nothing -> issueAt producer MediaTypeSchemaRequired
          Just prodRef -> localStep MediaTypeSchema
            $ checkCompatibility env $ ProdCons prodRef consRef
    pure ()
    where
      mediaType = getH @MediaType env
      checkEncoding =
        let
          -- Parameters of the media type are product-like entities
          getEncoding mt = M.fromList
            $ (IOHM.toList $ _mediaTypeObjectEncoding mt) <&> \(k, enc) ->
            ( k
            , ProductLike
              { traced = Traced (step $ MediaTypeParamEncoding k) enc
              , required = True } )
          encProdCons = getEncoding <$> prodCons
        in checkProducts MediaEncodingMissing
           (const $ checkCompatibility HNil) encProdCons

instance Subtree Encoding where
  type CheckEnv Encoding = '[]
  data CheckIssue Encoding = EncodingNotSupported
    --  FIXME: Support only JSON body for now. Then Encoding is checked only for
    --  multipart/form-url-encoded
    deriving (Eq, Ord, Show)
  checkCompatibility _env _prodCons =
    issueAt producer EncodingNotSupported

instance Steppable MediaTypeObject (Referenced Schema) where
  data Step MediaTypeObject (Referenced Schema) = MediaTypeSchema
    deriving (Eq, Ord, Show)

instance Steppable MediaTypeObject Encoding where
  data Step MediaTypeObject Encoding = MediaTypeParamEncoding Text
    deriving (Eq, Ord, Show)
