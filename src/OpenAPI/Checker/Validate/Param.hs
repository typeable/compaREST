{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Param () where

import Control.Monad
import Data.Maybe
import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree Param where
  type CheckEnv Param = '[]
  data CheckIssue Param
    = ParamNameMismatch
    -- ^ Params have different names
    | ParamEmptinessIncompatible
    -- ^ Consumer requires non-empty param, but producer gives emptyable
    | ParamRequired
    -- ^ Consumer requires mandatory parm, but producer optional
    | ParamPlaceIncompatible
    deriving (Eq, Ord, Show)
  checkCompatibility _ (ProdCons p c) = do
    when (_paramName p /= _paramName c)
      $ issueAt producer ParamNameMismatch
    when ((fromMaybe False $ _paramRequired c) &&
          not (fromMaybe False $ _paramRequired p))
      $ issueAt producer ParamRequired
    case (_paramIn p, _paramIn c) of
      (ParamQuery, ParamQuery) -> do
        -- Emptiness is only for query params
        when ((fromMaybe False $ _paramAllowEmptyValue p)
              && not (fromMaybe False $ _paramAllowEmptyValue c))
          $ issueAt producer ParamEmptinessIncompatible
      (a, b) | a == b -> pure ()
      _ -> issueAt producer ParamPlaceIncompatible
    pure ()
