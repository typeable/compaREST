{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Param () where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree Param where
  type CheckEnv Param = '[]
  data CheckIssue Param
    deriving (Eq, Ord)
  normalizeTrace = undefined
  checkCompatibility = undefined
