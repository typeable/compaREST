{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.SecurityRequirement
  ( Issue (..),
  )
where

import Control.Comonad
import Control.Monad
import Control.Monad.Writer
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Functor
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List.NonEmpty as NE
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.OAuth2Flows
import Data.OpenApi.Compare.Validate.SecurityScheme ()
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Traversable

instance Subtree SecurityRequirement where
  type SubtreeLevel SecurityRequirement = 'SecurityRequirementLevel
  type
    CheckEnv SecurityRequirement =
      '[ ProdCons (Traced (Definitions SecurityScheme))
       ]
  checkStructuralCompatibility env pc = do
    let normalized = do
          sec <- extract <$> pc
          defs <- getH env
          -- lookupScheme
          pure $
            for (IOHM.toList $ getSecurityRequirement sec) $ \(key, scopes) ->
              (,scopes) <$> lookupScheme key defs
    structuralMaybeWith
      ( \pc' -> do
          let ProdCons pScopes cScopes = fmap snd <$> pc'
          unless (pScopes == cScopes) structuralIssue
          structuralList env $ fmap fst <$> pc'
          pure ()
      )
      normalized
    pure ()
  checkSemanticCompatibility env bhv' pc = do
    let schemes = getH @(ProdCons (Traced (Definitions SecurityScheme))) env
        ( ProdCons pErrs cErrs
          , (ProdCons pSchemes cSchemes) ::
              ProdCons [(Behavior 'SecuritySchemeLevel, Traced SecurityScheme, [Text])]
          ) =
            NE.unzip $
              partitionEithers <$> do
                req <- pc
                scheme <- schemes
                pure $
                  let -- [(key, scopes)]
                      pairs = IOHM.toList . getSecurityRequirement . extract $ req
                   in pairs <&> \(key, scopes) -> do
                        case lookupScheme key scheme of
                          Nothing -> Left $ UndefinedSecurityScheme key
                          Just x -> Right (bhv' >>> step (SecuritySchemeStep key), x, scopes)
        lookSimilar :: SecurityScheme -> SecurityScheme -> Bool
        lookSimilar x y = case (_securitySchemeType x, _securitySchemeType y) of
          (SecuritySchemeOAuth2 {}, SecuritySchemeOAuth2 {}) -> True
          (SecuritySchemeHttp {}, SecuritySchemeHttp {}) -> True
          (SecuritySchemeApiKey {}, SecuritySchemeApiKey {}) -> True
          (SecuritySchemeOpenIdConnect {}, SecuritySchemeOpenIdConnect {}) -> True
          _ -> False
    issueAt bhv' `traverse_` pErrs
    issueAt bhv' `traverse_` cErrs
    for_ pSchemes $ \(bhv, pScheme, pScopes) -> do
      let lookPromising = filter (lookSimilar (extract pScheme) . extract . (\(_, x, _) -> x)) cSchemes
      anyOfAt bhv SecuritySchemeNotMatched $
        lookPromising <&> \(_, cScheme, cScopes) -> do
          let untracedSchemes = join bimap (_securitySchemeType . extract) (pScheme, cScheme)
              scopedFlow :: Set Text -> OAuth2Flow t -> Writer [Issue 'SecuritySchemeLevel] (OAuth2Flow t)
              scopedFlow scopes x = do
                let scopesMap = _oAuth2Scopes x
                for_ scopes $ \scope -> unless (scope `IOHM.member` scopesMap) $ tell [ScopeNotDefined scope]
                pure $ x {_oAuth2Scopes = IOHM.filterWithKey (\k _ -> k `S.member` scopes) scopesMap}
              scopedSchemeType :: [Text] -> SecuritySchemeType -> Writer [Issue 'SecuritySchemeLevel] SecuritySchemeType
              scopedSchemeType scopes (SecuritySchemeOAuth2 (OAuth2Flows a b c d)) =
                fmap SecuritySchemeOAuth2 $ OAuth2Flows <$> flow a <*> flow b <*> flow c <*> flow d
                where
                  flow :: Maybe (OAuth2Flow t) -> Writer [Issue 'SecuritySchemeLevel] (Maybe (OAuth2Flow t))
                  flow = traverse $ scopedFlow $ S.fromList scopes
              scopedSchemeType _ x = pure x
              scopedScheme scopes x = do
                sType <- scopedSchemeType scopes $ _securitySchemeType x
                pure $ x {_securitySchemeType = sType}
              (pc', errs) = runWriter $ ProdCons <$> scopedScheme pScopes `traverse` pScheme <*> scopedScheme cScopes `traverse` cScheme
          case untracedSchemes of
            (SecuritySchemeOpenIdConnect _, SecuritySchemeOpenIdConnect _) -> do
              let missingScopes = S.fromList cScopes S.\\ S.fromList pScopes
              unless (S.null missingScopes) $ issueAt bhv (ScopesMissing missingScopes)
            (SecuritySchemeOAuth2 {}, SecuritySchemeOAuth2 {}) -> pure ()
            _ -> unless (null pScopes && null cScopes) $ issueAt bhv CanNotHaveScopes
          for_ errs $ issueAt bhv
          checkCompatibility bhv env pc'
          pure ()
    pure ()

lookupScheme :: Text -> Traced (Definitions SecurityScheme) -> Maybe (Traced SecurityScheme)
lookupScheme k (Traced t m) = Traced (t >>> step (InsOrdHashMapKeyStep k)) <$> IOHM.lookup k m
