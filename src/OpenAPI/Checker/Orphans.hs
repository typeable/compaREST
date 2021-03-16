{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Orphans (Step (..)) where

import Data.OpenApi
import Data.Typeable
import OpenAPI.Checker.Trace

deriving newtype instance Ord Reference

instance Typeable a => Steppable (Referenced a) a where
  data Step (Referenced a) a
    = InlineStep
    | ReferencedStep Reference
    deriving (Eq, Ord)
