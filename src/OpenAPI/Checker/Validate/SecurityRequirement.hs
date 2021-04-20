{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.SecurityRequirement
  ( CheckIssue (..)
  ) where

import Data.OpenApi
import OpenAPI.Checker.Subtree

instance Subtree SecurityRequirement where
  type
    CheckEnv SecurityRequirement =
      '[ ProdCons (Definitions SecurityScheme)
       ]
  data CheckIssue SecurityRequirement
    = SecurityRequirementNotMet
    deriving (Eq, Ord, Show)
  checkCompatibility = undefined
