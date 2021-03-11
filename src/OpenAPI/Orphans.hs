{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Orphans () where

import Data.OpenApi
import GHC.Generics (Generic)

deriving stock instance Generic (Referenced v)

deriving stock instance Generic SecurityRequirement

deriving stock instance Generic AdditionalProperties

deriving stock instance Generic OpenApiItems
