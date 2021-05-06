{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Server
  ( CheckIssue(..)
  ) where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree Server where
  type CheckEnv Server = '[]
  data CheckIssue Server
    = ServerNotConsumed
    deriving (Eq, Ord, Show)
  checkCompatibility = undefined
