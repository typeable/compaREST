{-# OPTIONS_GHC -Wno-orphans #-}
-- Does not compiles otherwise
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Data.OpenApi.Compare.Validate.OpenApi
  ( Behave (..),
    Issue (..),
  )
where

import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Paths
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Operation

tracedPaths :: Traced OpenApi -> Traced ProcessedPathItems
tracedPaths oa =
  traced
    (ask oa >>> step OpenApiPathsStep)
    (processPathItems . IOHM.toList . _openApiPaths . extract $ oa)

tracedRequestBodies :: Traced OpenApi -> Traced (Definitions RequestBody)
tracedRequestBodies oa =
  traced
    (ask oa >>> step ComponentsRequestBody)
    (_componentsRequestBodies . _openApiComponents . extract $ oa)

tracedParameters :: Traced OpenApi -> Traced (Definitions Param)
tracedParameters oa =
  traced
    (ask oa >>> step ComponentsParam)
    (_componentsParameters . _openApiComponents . extract $ oa)

tracedSecuritySchemes :: Traced OpenApi -> Traced (Definitions SecurityScheme)
tracedSecuritySchemes oa =
  traced
    (ask oa >>> step ComponentsSecurityScheme)
    (_componentsSecuritySchemes . _openApiComponents . extract $ oa)

tracedResponses :: Traced OpenApi -> Traced (Definitions Response)
tracedResponses oa =
  traced
    (ask oa >>> step ComponentsResponse)
    (_componentsResponses . _openApiComponents . extract $ oa)

tracedHeaders :: Traced OpenApi -> Traced (Definitions Header)
tracedHeaders oa =
  traced
    (ask oa >>> step ComponentsHeader)
    (_componentsHeaders . _openApiComponents . extract $ oa)

tracedSchemas :: Traced OpenApi -> Traced (Definitions Schema)
tracedSchemas oa =
  traced
    (ask oa >>> step ComponentsSchema)
    (_componentsSchemas . _openApiComponents . extract $ oa)

tracedLinks :: Traced OpenApi -> Traced (Definitions Link)
tracedLinks oa =
  traced
    (ask oa >>> step ComponentsLink)
    (_componentsLinks . _openApiComponents . extract $ oa)

tracedCallbacks :: Traced OpenApi -> Traced (Definitions Callback)
tracedCallbacks (Traced t x) =
  Traced
    (t >>> step ComponentsCallbacks)
    (_componentsCallbacks . _openApiComponents $ x)

instance Subtree OpenApi where
  type SubtreeLevel OpenApi = 'APILevel
  type CheckEnv OpenApi = '[]

  -- There is no real reason to do a proper implementation
  checkStructuralCompatibility _ _ = structuralIssue
  checkSemanticCompatibility _ beh prodCons = do
    checkCompatibility @ProcessedPathItems
      beh
      ( (tracedRequestBodies <$> prodCons)
          `HCons` (tracedParameters <$> prodCons)
          `HCons` (tracedSecuritySchemes <$> prodCons)
          `HCons` (tracedResponses <$> prodCons)
          `HCons` (tracedHeaders <$> prodCons)
          `HCons` (tracedSchemas <$> prodCons)
          `HCons` (_openApiServers . extract <$> prodCons)
          `HCons` (tracedLinks <$> prodCons)
          `HCons` (tracedCallbacks <$> prodCons)
          `HCons` HNil
      )
      (tracedPaths <$> prodCons)

instance Steppable OpenApi ProcessedPathItems where
  data Step OpenApi ProcessedPathItems = OpenApiPathsStep
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions RequestBody) where
  data Step OpenApi (Definitions RequestBody) = ComponentsRequestBody
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Param) where
  data Step OpenApi (Definitions Param) = ComponentsParam
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions SecurityScheme) where
  data Step OpenApi (Definitions SecurityScheme) = ComponentsSecurityScheme
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Response) where
  data Step OpenApi (Definitions Response) = ComponentsResponse
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Header) where
  data Step OpenApi (Definitions Header) = ComponentsHeader
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Schema) where
  data Step OpenApi (Definitions Schema) = ComponentsSchema
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Link) where
  data Step OpenApi (Definitions Link) = ComponentsLink
    deriving stock (Eq, Ord, Show)

instance Steppable OpenApi (Definitions Callback) where
  data Step OpenApi (Definitions Callback) = ComponentsCallbacks
    deriving stock (Eq, Ord, Show)
