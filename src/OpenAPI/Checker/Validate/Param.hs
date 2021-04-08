{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Param () where

import Data.OpenApi
import Data.Maybe
import OpenAPI.Checker.Subtree

instance Subtree Param where
  type CheckEnv Param = '[]
  data CheckIssue Param
    = ParamNameMismatch
    -- ^ Params have different names
    | ParamOptionalityIncompatible
    -- ^ Consumer requires non-empty param, but producer gives optional
    | ParamPlaceIncompatible
    deriving (Eq, Ord, Show)
  checkCompatibility _ (ProdCons p c) = case ( _paramIn p, _paramIn c) of
    (ParamQuery, ParamQuery) ->
      namesCompatible $
        if (fromMaybe False $ _paramAllowEmptyValue p)
           && not (fromMaybe False $ _paramAllowEmptyValue c)
        then issueAt producer ParamOptionalityIncompatible
        else pure ()
    (a, b) | a == b -> namesCompatible $ pure ()
    _ -> issueAt producer ParamPlaceIncompatible
    where
      namesCompatible next =
        if _paramName p /= _paramName c
        then issueAt producer ParamNameMismatch
        else next
