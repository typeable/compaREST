{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Operation (Step (..)) where

import Data.OpenApi
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace

instance Subtree Operation where
  type CheckEnv Operation = '[]
  data CheckIssue Operation
    deriving (Eq, Ord, Show)
  normalizeTrace = undefined
  checkCompatibility = undefined

instance Steppable PathItem Operation where
  data Step PathItem Operation
    = GetStep
    | PutStep
    | PostStep
    | DeleteStep
    | OptionsStep
    | HeadStep
    | PatchStep
    | TraceStep
    deriving (Eq, Ord, Show)
