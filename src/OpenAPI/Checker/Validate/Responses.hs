{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Control.Monad
import Data.Foldable
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject ()
import OpenAPI.Checker.Validate.Products
import OpenAPI.Checker.Validate.Schema ()
import OpenAPI.Checker.Validate.Sums

instance Subtree Responses where
  type CheckEnv Responses =
    '[ ProdCons (Definitions Response)
     , ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Responses = ResponseCodeNotFound HttpStatusCode
    deriving (Eq, Ord, Show)
  -- Roles are already swapped. Producer is a server and consumer is a
  -- client. Response codes are sum-like entity because we can answer with only
  -- one element
  checkCompatibility env prodCons = do
    let
      defs = getH @(ProdCons (Definitions Response)) env
      check _ responses = checkCompatibility @Response env responses
      elements = getEls <$> defs <*> prodCons
      getEls respDef resps = M.fromList $ do
        (code, respRef) <- IOHM.toList $ _responsesResponses resps
        let
          traced = dereferenceTraced respDef
            $ Traced (step $ ResponseCodeStep code) respRef
        pure (code, traced)
    checkSums ResponseCodeNotFound check elements

instance Subtree Response where
  type CheckEnv Response =
    '[ ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue Response
    = ResponseMediaTypeMissing MediaType
    | ResponseHeaderMissing HeaderName
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons@(ProdCons p c) = do
    -- Roles are already swapped. Producer is a server and consumer is a client
    checkMediaTypes
    checkHeaders
    pure ()
    where
      checkMediaTypes = do
        -- Media types are sum-like entity
        let
          check mediaType mtObj =
            let mtEnv = HCons mediaType $ env
            in checkCompatibility @MediaTypeObject mtEnv mtObj
          elements = getEls <$> prodCons
          getEls resp = M.fromList $ do
            (mediaType, mtObj) <- IOHM.toList $ _responseContent resp
            let traced = Traced (step $ ResponseMediaObject mediaType) mtObj
            pure (mediaType, traced)
        checkSums ResponseMediaTypeMissing check elements

      checkHeaders = do
        -- Headers are product-like entities
        let
          check _hname headers = checkCompatibility @Header env headers
          elements = getEls <$> headerDefs <*> prodCons
          getEls headerDef resp = M.fromList $ do
            (hname, headerRef) <- IOHM.toList $ _responseHeaders resp
            let
              traced = dereferenceTraced headerDef
                $ Traced (step $ ResponseHeader hname) headerRef
              required = fromMaybe False $ _headerRequired $ getTraced traced
              elt = ProductLike { traced, required }
            pure (hname, elt)
        checkProducts ResponseHeaderMissing check elements
      headerDefs = getH @(ProdCons (Definitions Header)) env

instance Subtree Header where
  type CheckEnv Header = '[ProdCons (Definitions Schema)]
  data CheckIssue Header
    = RequiredHeaderMissing
    | NonEmptyHeaderRequired
    | HeaderSchemaRequired
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    if (fromMaybe False $ _headerRequired c) && not (fromMaybe False $ _headerRequired p)
      then issueAt producer RequiredHeaderMissing else pure ()
    if not (fromMaybe False $ _headerAllowEmptyValue c) && (fromMaybe False $ _headerAllowEmptyValue p)
      then issueAt producer NonEmptyHeaderRequired else pure ()
    for_ (_headerSchema c) $ \consRef ->
      case (_headerSchema p) of
        Nothing -> issueAt producer HeaderSchemaRequired
        Just prodRef -> do
          localStep HeaderSchema
            $ checkCompatibility env $ ProdCons prodRef consRef
    pure ()

instance Steppable Header (Referenced Schema) where
  data Step Header (Referenced Schema) = HeaderSchema
    deriving (Eq, Ord, Show)

instance Steppable Response (Referenced Header) where
  data Step Response (Referenced Header) = ResponseHeader HeaderName
    deriving (Eq, Ord, Show)

instance Steppable Response MediaTypeObject where
  data Step Response MediaTypeObject = ResponseMediaObject MediaType
    deriving (Eq, Ord, Show)

instance Steppable Responses (Referenced Response) where
  data Step Responses (Referenced Response) = ResponseCodeStep HttpStatusCode
    deriving (Eq, Ord, Show)
