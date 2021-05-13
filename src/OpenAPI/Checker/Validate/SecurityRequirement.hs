{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.SecurityRequirement
  ( Issue (..)
  ) where

import Data.OpenApi
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree

instance Issuable 'SecurityRequirementLevel where
  data Issue 'SecurityRequirementLevel
    = SecurityRequirementNotMet
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Subtree SecurityRequirement where
  type SubtreeLevel SecurityRequirement = 'SecurityRequirementLevel
  type
    CheckEnv SecurityRequirement =
      '[ ProdCons (Traced (Definitions SecurityScheme))
       ]
  checkCompatibility = undefined
