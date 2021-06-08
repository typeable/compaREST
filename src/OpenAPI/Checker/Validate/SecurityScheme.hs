{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.SecurityScheme
  (
  )
where

import Control.Monad
import Data.OpenApi
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OAuth2Flows

instance Subtree SecurityScheme where
  type CheckEnv SecurityScheme = '[]
  type SubtreeLevel SecurityScheme = 'SecuritySchemeLevel
  checkStructuralCompatibility _ pc = structuralEq $ tracedSecuritySchemaTypes <$> pc
  checkSemanticCompatibility env bhv pcSecScheme = case tracedSecuritySchemaTypes <$> pcSecScheme of
    (ProdCons (Traced _ (SecuritySchemeHttp pType)) (Traced _ (SecuritySchemeHttp cType))) -> case (pType, cType) of
      (HttpSchemeBearer _, HttpSchemeBearer _) -> pure ()
      (HttpSchemeBasic, HttpSchemeBasic) -> pure ()
      (HttpSchemeCustom p, HttpSchemeCustom c) ->
        unless (p == c) (issueAt bhv $ CustomHttpSchemesDontMatch p c)
      _ -> issueAt bhv $ HttpSchemeTypesDontMatch pType cType
    (ProdCons (Traced _ (SecuritySchemeApiKey pParams)) (Traced _ (SecuritySchemeApiKey cParams))) -> do
      unless (pParams == cParams) (issueAt bhv $ ApiKeyParamsDontMatch pParams cParams)
    (ProdCons (Traced pT (SecuritySchemeOAuth2 pFlows)) (Traced cT (SecuritySchemeOAuth2 cFlows))) -> do
      checkCompatibility env bhv . fmap (stepTraced SecurityOAuthFlowsStep) $ ProdCons (Traced pT pFlows) (Traced cT cFlows)
    (ProdCons (Traced _ (SecuritySchemeOpenIdConnect pUrl)) (Traced _ (SecuritySchemeOpenIdConnect cUrl))) -> do
      unless (pUrl == cUrl) (issueAt bhv $ OpenIdConnectUrlsDontMatch pUrl cUrl)
    _ -> issueAt bhv DifferentSecuritySchemes

tracedSecuritySchemaTypes :: Traced SecurityScheme -> Traced SecuritySchemeType
tracedSecuritySchemaTypes (Traced t x) = Traced (t >>> step SecuritySchemeTypeStep) (_securitySchemeType x)

instance Steppable SecurityScheme SecuritySchemeType where
  data Step SecurityScheme SecuritySchemeType = SecuritySchemeTypeStep
    deriving stock (Eq, Ord, Show)

instance Steppable SecuritySchemeType OAuth2Flows where
  data Step SecuritySchemeType OAuth2Flows = SecurityOAuthFlowsStep
    deriving stock (Eq, Ord, Show)
