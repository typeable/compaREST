{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Orphans () where

import Data.OpenApi

deriving newtype instance Ord Reference
