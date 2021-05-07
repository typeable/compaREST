{-# OPTIONS_GHC -Wno-orphans #-}
-- Does not compiles otherwise
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

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

tracedPaths :: Traced r OpenApi -> Traced r ProcessedPathItems
tracedPaths oa = traced (ask oa >>> step OpenApiPathsStep)
  (processPathItems . IOHM.toList . _openApiPaths . extract $ oa)

instance Subtree OpenApi where
  type CheckEnv OpenApi = '[]
  data CheckIssue OpenApi
    deriving (Eq, Ord, Show)
  checkCompatibility _ prodCons = do
    let cs = _openApiComponents . extract <$> prodCons
    checkCompatibility @ProcessedPathItems
      ((_componentsRequestBodies <$> cs)
         `HCons` (_componentsParameters <$> cs)
         `HCons` (_componentsSecuritySchemes <$> cs)
         `HCons` (_componentsResponses <$> cs)
         `HCons` (_componentsHeaders <$> cs)
         `HCons` (_componentsSchemas <$> cs)
         `HCons` (_openApiServers . extract <$> prodCons)
         `HCons` HNil)
      (tracedPaths <$> prodCons)

instance Steppable OpenApi ProcessedPathItems where
  data Step OpenApi ProcessedPathItems = OpenApiPathsStep
    deriving (Eq, Ord, Show)
