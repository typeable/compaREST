{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Server
  (
  )
where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree Server where
  type CheckEnv Server = '[]
  data CheckIssue Server
    deriving (Eq, Ord, Show)
  checkCompatibility = undefined
