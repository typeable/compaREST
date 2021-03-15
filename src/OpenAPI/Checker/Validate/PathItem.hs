module OpenAPI.Checker.Validate.PathItem
  (
  )
where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree PathItem where
  type CheckEnv PathItem = '[]
  data CheckIssue PathItem
    deriving (Eq, Ord)
  normalizeTrace = undefined
