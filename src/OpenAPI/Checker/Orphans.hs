{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Orphans (Step (..)) where

import Data.OpenApi
import Data.Typeable
import qualified Data.HashMap.Strict.InsOrd as IOHM
import OpenAPI.Checker.Trace

deriving newtype instance Ord Reference

instance Typeable a => Steppable (Referenced a) a where
  data Step (Referenced a) a
    = InlineStep
    | ReferencedStep Reference
    deriving (Eq, Ord, Show)

deriving stock instance Ord a => Ord (Referenced a)
deriving stock instance Ord Schema
deriving stock instance Ord AdditionalProperties
deriving stock instance Ord Discriminator
deriving stock instance Ord Xml
deriving stock instance Ord OpenApiType
deriving stock instance Ord OpenApiItems

instance (Ord k, Ord v) => Ord (IOHM.InsOrdHashMap k v) where
  compare xs ys = compare (IOHM.toList xs) (IOHM.toList ys)
