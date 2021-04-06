module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree Responses where
  type CheckEnv Responses = '[]
  data CheckIssue Responses
    deriving (Eq, Ord, Show)
  checkCompatibility = undefined
