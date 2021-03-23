{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.RequestBody
  (
  )
where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree RequestBody where
  type CheckEnv RequestBody = '[]
  data CheckIssue RequestBody
    deriving (Eq, Ord, Show)
  normalizeTrace = undefined
  checkCompatibility = undefined
