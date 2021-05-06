{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  ( CheckIssue (..)
  )
where

import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.Sums

tracedContent :: Traced r RequestBody -> IOHM.InsOrdHashMap MediaType (Traced r MediaTypeObject)
tracedContent resp = IOHM.mapWithKey (\k -> traced (ask resp >>> step (RequestMediaTypeObject k)))
  $ _requestBodyContent $ extract resp

instance Subtree RequestBody where
  type CheckEnv RequestBody =
    '[ ProdCons (Definitions Schema) ]
  data CheckIssue RequestBody
    = NoRequestBody
    | RequestBodyRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons@(ProdCons p c) =
    if not (fromMaybe False . _requestBodyRequired . extract $ p)
        && (fromMaybe False . _requestBodyRequired . extract $ c)
    then issueAt p RequestBodyRequired
    else
      -- Media type object are sums-like entities.
      let
        check mediaType pc = checkCompatibility @MediaTypeObject (HCons mediaType env) pc
        sumElts = getSum <$> prodCons
        getSum rb = M.fromList . IOHM.toList $ tracedContent rb
      in checkSums (const RequestMediaTypeNotFound) check sumElts

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = RequestMediaTypeObject MediaType
    deriving (Eq, Ord, Show)
