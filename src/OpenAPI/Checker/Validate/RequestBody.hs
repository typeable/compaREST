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
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject ()

instance Subtree RequestBody where
  type CheckEnv RequestBody =
    '[ ProdCons (Definitions Schema) ]
  data CheckIssue RequestBody
    = RequestBodyRequired
    | RequestMediaTypeNotFound MediaType
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) =
    if not (fromMaybe False $ _requestBodyRequired p)
        && (fromMaybe False $ _requestBodyRequired c)
    then issueAt producer RequestBodyRequired
    else
      -- For each consumer we must find at least one compatible producer media
      -- type
      for_ (IOHM.toList $ _requestBodyContent p) $ \(mediaType, prodMedia) ->
      case IOHM.lookup mediaType $ _requestBodyContent c of
        Nothing -> issueAt consumer (RequestMediaTypeNotFound mediaType)
        Just consMedia -> localStep (MediaTypeStep mediaType) $
          checkCompatibility (HCons mediaType env) (ProdCons prodMedia consMedia)

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = MediaTypeStep MediaType
    deriving (Eq, Ord, Show)
