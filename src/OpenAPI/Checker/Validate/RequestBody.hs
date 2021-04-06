{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  (
  )
where

import Data.Foldable as F
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import Network.HTTP.Media (MediaType, mainType, subType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace

instance Subtree RequestBody where
  type CheckEnv RequestBody = '[]
  data CheckIssue RequestBody
    = RequestBodyRequired
    | RequestMediaTypeNotFound
    deriving (Eq, Ord, Show)
  checkCompatibility _ (ProdCons p c) =
    if not (fromMaybe False $ _requestBodyRequired p)
        && (fromMaybe False $ _requestBodyRequired c)
    then issueAt producer RequestBodyRequired
    else
      -- For each consumer we must find at least one compatible producer media
      -- type
      for_ (IOHM.toList $ _requestBodyContent c) $ \(mediaType, consMedia) ->
      case IOHM.lookup mediaType $ _requestBodyContent p of
        Nothing -> issueAt producer RequestMediaTypeNotFound
        Just prodMedia -> localStep (MediaTypeStep mediaType) $
          checkCompatibility (singletonH mediaType) (ProdCons prodMedia consMedia)


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

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = MediaTypeStep MediaType
    deriving (Eq, Ord, Show)

instance Steppable MediaTypeObject Encoding where
  data Step MediaTypeObject Encoding = MediaTypeParamEncoding Text
    deriving (Eq, Ord, Show)
