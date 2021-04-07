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

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = MediaTypeStep MediaType
    deriving (Eq, Ord, Show)
