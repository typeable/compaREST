{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.OpenApi
  (
  )
where

import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.ProcessedPathItem

instance Subtree OpenApi where
  type CheckEnv OpenApi = '[]
  data CheckIssue OpenApi
    deriving (Eq, Ord, Show)
  checkCompatibility _ prodCons = do
    let cs = _openApiComponents <$> prodCons
    localStep OpenApiPathsStep $
      checkCompatibility
        ((_componentsRequestBodies <$> cs)
           `HCons` (_componentsParameters <$> cs)
           `HCons` (_componentsSecuritySchemes <$> cs)
           `HCons` (_componentsResponses <$> cs)
           `HCons` HNil)
        (processPathItems . IOHM.toList . _openApiPaths <$> prodCons)

instance Steppable OpenApi ProcessedPathItems where
  data Step OpenApi ProcessedPathItems = OpenApiPathsStep
    deriving (Eq, Ord, Show)
