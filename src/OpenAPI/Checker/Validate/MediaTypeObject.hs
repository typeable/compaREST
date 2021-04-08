{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.MediaTypeObject () where

import Data.Foldable as F
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import Data.Text (Text)
import Network.HTTP.Media (MediaType, mainType, subType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Schema

instance Subtree MediaTypeObject where
  type CheckEnv MediaTypeObject =
    '[ MediaType
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue MediaTypeObject
    = MediaEncodingMissing
    | MediaEncodingIncompat
    | MediaTypeSchemaRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    tryCheckEncoding
    checkSchema
    pure ()
    where
      mediaType = getH @MediaType env
      tryCheckEncoding =
        if | "multipart" == mainType mediaType -> checkEncoding
           | "application" == mainType mediaType &&
             "x-www-form-urlencoded" == subType mediaType -> checkEncoding
           | otherwise -> pure ()
        where
          checkEncoding = for_ (IOHM.toList $ _mediaTypeObjectEncoding c) $ \(paramName, consEncoding) ->
            case IOHM.lookup paramName $ _mediaTypeObjectEncoding p of
              Nothing -> issueAt producer MediaEncodingMissing
              Just prodEncoding -> localStep (MediaTypeParamEncoding paramName)
                $ checkCompatibility HNil
                $ ProdCons prodEncoding consEncoding
      checkSchema = for_ (_mediaTypeObjectSchema c) $ \consRef ->
        case _mediaTypeObjectSchema p of
          Nothing -> issueAt producer MediaTypeSchemaRequired
          Just prodRef -> localStep MediaTypeSchema
            $ checkCompatibility env $ ProdCons prodRef consRef

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
