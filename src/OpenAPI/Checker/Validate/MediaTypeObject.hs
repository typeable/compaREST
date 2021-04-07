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


instance Subtree MediaTypeObject where
  type CheckEnv MediaTypeObject = '[MediaType]
  data CheckIssue MediaTypeObject
    = MediaEncodingMissing
    | MediaEncodingIncompat
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = sequenceA_
    [ tryCheckEncoding
    , checkSchema ]
    where
      mediaType = getH @MediaType env
      tryCheckEncoding =
        if | "multipart" == mainType mediaType -> checkEncoding
           | "application" == mainType mediaType &&
             "x-www-form-urlencoded" == subType mediaType -> checkEncoding
           | otherwise -> pure ()
      checkEncoding = for_ (IOHM.toList $ _mediaTypeObjectEncoding c) $ \(paramName, consEncoding) ->
        case IOHM.lookup paramName $ _mediaTypeObjectEncoding p of
          Nothing -> issueAt producer MediaEncodingMissing
          Just prodEncoding -> localStep (MediaTypeParamEncoding paramName)
            $ checkCompatibility HNil
            $ ProdCons prodEncoding consEncoding
      checkSchema = (error "FIXME: check schema must be implemented later")

instance Subtree Encoding where
  type CheckEnv Encoding = '[]
  data CheckIssue Encoding = EncodingNotSupported
    --  FIXME: Support only JSON body for now. Then Encoding is checked only for
    --  multipart/form-url-encoded
    deriving (Eq, Ord, Show)
  checkCompatibility _env _prodCons =
    issueAt producer EncodingNotSupported

instance Steppable MediaTypeObject Encoding where
  data Step MediaTypeObject Encoding = MediaTypeParamEncoding Text
    deriving (Eq, Ord, Show)
