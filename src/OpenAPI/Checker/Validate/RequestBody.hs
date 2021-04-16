{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  (
  )
where

import Data.Functor
import Data.HList
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Network.HTTP.Media (MediaType)
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.MediaTypeObject ()
import OpenAPI.Checker.Validate.Sums

instance Subtree RequestBody where
  type CheckEnv RequestBody =
    '[ ProdCons (Definitions Schema) ]
  data CheckIssue RequestBody
    = RequestBodyRequired
    | RequestMediaTypeNotFound MediaType
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons@(ProdCons p c) =
    if not (fromMaybe False $ _requestBodyRequired p)
        && (fromMaybe False $ _requestBodyRequired c)
    then issueAt producer RequestBodyRequired
    else
      -- Media type object are sums-like entities.
      let
        check mediaType pc = checkCompatibility @MediaTypeObject (HCons mediaType env) pc
        sumElts = getSum <$> prodCons
        getSum rb = M.fromList
          $ (IOHM.toList $ _requestBodyContent rb) <&> \(mt, mto) ->
          ( mt
          , SumLike
            { value = mto
            , eltStep = RequestMediaTypeObject mt })
      in checkSums RequestMediaTypeNotFound check sumElts

instance Steppable RequestBody MediaTypeObject where
  data Step RequestBody MediaTypeObject = RequestMediaTypeObject MediaType
    deriving (Eq, Ord, Show)
