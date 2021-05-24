{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.OAuth2Flows
  ( Step (..)
  , Issue (..)
  , Behave (..)
  )
where

import Control.Monad
import Data.Function
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Subtree

instance Subtree OAuth2Flows where
  type CheckEnv OAuth2Flows = '[]
  type SubtreeLevel OAuth2Flows = 'SecuritySchemeLevel
  checkStructuralCompatibility _ = structuralEq
  checkSemanticCompatibility env bhv pc = do
    let supportFlow
          :: (Subtree t, SubtreeLevel t ~ SubtreeLevel OAuth2Flows, CheckEnv OAuth2Flows ~ CheckEnv t)
          => Issue 'SecuritySchemeLevel
          -> ProdCons (Maybe (Traced t))
          -> SemanticCompatFormula ()
        supportFlow i x = case x of
          -- producer will not attempt this flow
          (ProdCons Nothing _) -> pure ()
          -- producer can attempt a flow the consumer does not know about
          (ProdCons (Just _) Nothing) -> issueAt bhv i
          (ProdCons (Just p) (Just c)) ->
            checkCompatibility env bhv $ ProdCons p c
        getFlow
          :: Typeable x
          => (OAuth2Flows -> Maybe (OAuth2Flow x))
          -> Traced OAuth2Flows
          -> Maybe (Traced (OAuth2Flow x))
        getFlow f (Traced t a) = Traced (t >>> step (OAuth2FlowsFlow Proxy)) <$> f a
    supportFlow ConsumerDoesNotSupportImplicitFlow $ getFlow _oAuth2FlowsImplicit <$> pc
    supportFlow ConsumerDoesNotSupportPasswordFlow $ getFlow _oAuth2FlowsPassword <$> pc
    supportFlow ConsumerDoesNotSupportClientCridentialsFlow $ getFlow _oAuth2FlowsClientCredentials <$> pc
    supportFlow ConsumerDoesNotSupportAuthorizationCodeFlow $ getFlow _oAuth2FlowsAuthorizationCode <$> pc
    pure ()

instance Typeable t => Steppable OAuth2Flows (OAuth2Flow t) where
  data Step OAuth2Flows (OAuth2Flow t) = OAuth2FlowsFlow (Proxy t)
    deriving (Eq, Ord, Show)

instance (Typeable t, Subtree t, SubtreeLevel (OAuth2Flow t) ~ SubtreeLevel t) => Subtree (OAuth2Flow t) where
  type CheckEnv (OAuth2Flow t) = CheckEnv t
  type SubtreeLevel (OAuth2Flow t) = 'SecuritySchemeLevel
  checkStructuralCompatibility = undefined
  checkSemanticCompatibility env bhv prodCons@(ProdCons p c) = do
    let ProdCons pScopes cScopes = S.fromList . IOHM.keys . _oAuth2Scopes . extract <$> prodCons
        missingScopes = cScopes S.\\ pScopes
    unless (S.null missingScopes) (issueAt bhv $ ScopesMissing missingScopes)
    checkCompatibility env bhv $ retraced (>>> step (OAuth2FlowParamsStep Proxy)) . fmap _oAuth2Params <$> prodCons
    unless (((==) `on` _oAath2RefreshUrl . extract) p c) $ issueAt bhv RefreshUrlsDontMatch
    pure ()

instance Typeable t => Steppable (OAuth2Flow t) t where
  data Step (OAuth2Flow t) t = OAuth2FlowParamsStep (Proxy t)
    deriving (Eq, Ord, Show)

instance Subtree OAuth2ImplicitFlow where
  type SubtreeLevel OAuth2ImplicitFlow = 'SecuritySchemeLevel
  type CheckEnv OAuth2ImplicitFlow = '[]
  checkStructuralCompatibility = undefined
  checkSemanticCompatibility _ bhv (ProdCons p c) =
    unless (extract p == extract c) $ issueAt bhv OAuth2ImplicitFlowNotEqual

instance Subtree OAuth2PasswordFlow where
  type SubtreeLevel OAuth2PasswordFlow = 'SecuritySchemeLevel
  type CheckEnv OAuth2PasswordFlow = '[]
  checkStructuralCompatibility = undefined
  checkSemanticCompatibility _ bhv (ProdCons p c) =
    unless (extract p == extract c) $ issueAt bhv OAuth2PasswordFlowNotEqual

instance Subtree OAuth2ClientCredentialsFlow where
  type SubtreeLevel OAuth2ClientCredentialsFlow = 'SecuritySchemeLevel
  type CheckEnv OAuth2ClientCredentialsFlow = '[]
  checkStructuralCompatibility = undefined
  checkSemanticCompatibility _ bhv (ProdCons p c) =
    unless (extract p == extract c) $ issueAt bhv OAuth2ClientCredentialsFlowNotEqual

instance Subtree OAuth2AuthorizationCodeFlow where
  type SubtreeLevel OAuth2AuthorizationCodeFlow = 'SecuritySchemeLevel
  type CheckEnv OAuth2AuthorizationCodeFlow = '[]
  checkStructuralCompatibility = undefined
  checkSemanticCompatibility _ bhv (ProdCons p c) =
    unless (extract p == extract c) $ issueAt bhv OAuth2AuthorizationCodeFlowNotEqual

instance Issuable 'SecurityRequirementLevel where
  data Issue 'SecurityRequirementLevel
    = SecurityRequirementNotMet
    | UndefinedSecurityScheme Text
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Issuable 'SecuritySchemeLevel where
  data Issue 'SecuritySchemeLevel
    = RefreshUrlsDontMatch
    | HttpSchemeTypesDontMatch HttpSchemeType HttpSchemeType
    | ApiKeyParamsDontMatch ApiKeyParams ApiKeyParams
    | OpenIdConnectUrlsDontMatch URL URL
    | CustomHttpSchemesDontMatch Text Text
    | ConsumerDoesNotSupportImplicitFlow
    | ConsumerDoesNotSupportPasswordFlow
    | ConsumerDoesNotSupportClientCridentialsFlow
    | ConsumerDoesNotSupportAuthorizationCodeFlow
    | SecuritySchemeNotMatched
    | OAuth2ImplicitFlowNotEqual
    | OAuth2PasswordFlowNotEqual
    | OAuth2ClientCredentialsFlowNotEqual
    | OAuth2AuthorizationCodeFlowNotEqual
    | ScopesMissing (Set Text)
    | DifferentSecuritySchemes
    | CanNotHaveScopes
    | ScopeNotDefined Text
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Behavable 'SecurityRequirementLevel 'SecuritySchemeLevel where
  data Behave 'SecurityRequirementLevel 'SecuritySchemeLevel
    = SecuritySchemeStep Text
    deriving stock (Eq, Ord, Show)