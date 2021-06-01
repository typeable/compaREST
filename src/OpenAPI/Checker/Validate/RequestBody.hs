{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  ( Issue (..)
  , Behave (..)
  )
where

import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import qualified Data.Text as T
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.MediaTypeObject
import OpenAPI.Checker.Validate.Sums
import Text.Pandoc.Builder

-- TODO: Use RequestMediaTypeObjectMapping
tracedContent :: Traced RequestBody -> IOHM.InsOrdHashMap MediaType (Traced MediaTypeObject)
tracedContent resp =
  IOHM.mapWithKey (\k -> traced (ask resp >>> step (RequestMediaTypeObject k))) $
    _requestBodyContent $ extract resp

instance Issuable 'RequestLevel where
  data Issue 'RequestLevel
    = RequestBodyRequired
    | RequestMediaTypeNotFound MediaType
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False
  describeIssue RequestBodyRequired =
    para "Expected the request body to be optional, but found it to be required."
  describeIssue (RequestMediaTypeNotFound t) =
    para $ "Couldn't find a request body for media type " <> (code . T.pack . show $ t) <> "."

instance Behavable 'RequestLevel 'PayloadLevel where
  data Behave 'RequestLevel 'PayloadLevel
    = InPayload
    deriving stock (Eq, Ord, Show)
  describeBehaviour InPayload = "Payload"

instance Subtree RequestBody where
  type SubtreeLevel RequestBody = 'RequestLevel
  type
    CheckEnv RequestBody =
      '[ ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Header))
       ]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _requestBodyRequired <$> pc
    iohmStructural env $
      stepTraced RequestMediaTypeObjectMapping . fmap _requestBodyContent <$> pc
    pure ()
  checkSemanticCompatibility env beh prodCons@(ProdCons p c) =
    if not (fromMaybe False . _requestBodyRequired . extract $ p)
      && (fromMaybe False . _requestBodyRequired . extract $ c)
      then issueAt beh RequestBodyRequired
      else -- Media type object are sums-like entities.

        let check mediaType pc = checkCompatibility @MediaTypeObject (HCons mediaType env) (beh >>> step InPayload) pc
            sumElts = getSum <$> prodCons
            getSum rb = M.fromList . IOHM.toList $ tracedContent rb
         in checkSums beh RequestMediaTypeNotFound check sumElts

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = RequestMediaTypeObject MediaType
    deriving (Eq, Ord, Show)

instance Steppable RequestBody (IOHM.InsOrdHashMap MediaType MediaTypeObject) where
  data Step RequestBody (IOHM.InsOrdHashMap MediaType MediaTypeObject) = RequestMediaTypeObjectMapping
    deriving (Eq, Ord, Show)
