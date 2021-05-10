{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  ( Issue (..)
  )
where

import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.Sums

tracedContent :: Traced RequestBody -> IOHM.InsOrdHashMap MediaType (Traced MediaTypeObject)
tracedContent resp = IOHM.mapWithKey (\k -> traced (ask resp >>> step (RequestMediaTypeObject k)))
  $ _requestBodyContent $ extract resp

instance Issuable 'RequestLevel where
  data Issue 'RequestLevel
    = RequestBodyRequired
    | RequestMediaTypeNotFound MediaType
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Behavable 'RequestLevel 'PayloadLevel where
  data Behave 'RequestLevel 'PayloadLevel
    = InPayload
    deriving stock (Eq, Ord, Show)

instance Subtree RequestBody where
  type SubtreeLevel RequestBody = 'RequestLevel
  type CheckEnv RequestBody =
    '[ ProdCons (Traced (Definitions Schema)) ]
  checkSemanticCompatibility env beh prodCons@(ProdCons p c) =
    if not (fromMaybe False . _requestBodyRequired . extract $ p)
        && (fromMaybe False . _requestBodyRequired . extract $ c)
    then issueAt beh RequestBodyRequired
    else
      -- Media type object are sums-like entities.
      let
        check mediaType pc = checkCompatibility @MediaTypeObject (HCons mediaType env) (beh >>> step InPayload) pc
        sumElts = getSum <$> prodCons
        getSum rb = M.fromList . IOHM.toList $ tracedContent rb
      in checkSums beh RequestMediaTypeNotFound check sumElts

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = RequestMediaTypeObject MediaType
    deriving (Eq, Ord, Show)
