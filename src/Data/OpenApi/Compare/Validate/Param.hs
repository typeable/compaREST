{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.OpenApi.Compare.Validate.Param
  ( Behave (..),
    Issue (..),
  )
where

import Control.Monad
import Data.Functor
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Orphans ()
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema ()
import Data.Text as T
import Text.Pandoc.Builder

-- | The type is normalized encoding style of the parameter. If two encoding
-- styles are equal then parameters are compatible with their encoding style
data EncodingStyle = EncodingStyle
  { style :: Style
  , explode :: Bool
  , -- | Nothing when @in@ parameter is not @query@
    allowReserved :: Maybe Bool
  }
  deriving stock (Eq, Ord, Show)

paramEncoding :: Param -> EncodingStyle
paramEncoding p =
  EncodingStyle
    { style
    , explode
    , allowReserved
    }
  where
    style = fromMaybe defaultStyle $ _paramStyle p
    defaultStyle = case _paramIn p of
      ParamQuery -> StyleForm
      ParamPath -> StyleSimple
      ParamHeader -> StyleSimple
      ParamCookie -> StyleForm
    explode = fromMaybe defaultExplode $ _paramExplode p
    defaultExplode = case style of
      StyleForm -> True
      _ -> False
    allowReserved = case _paramIn p of
      ParamQuery -> Just $ fromMaybe False $ _paramAllowReserved p
      _ -> Nothing

tracedSchema :: Traced Param -> Maybe (Traced (Referenced Schema))
tracedSchema par = _paramSchema (extract par) <&> traced (ask par >>> step ParamSchema)

instance Issuable 'PathFragmentLevel where
  data Issue 'PathFragmentLevel
    = -- | Params have different names
      ParamNameMismatch
    | -- | Consumer requires non-empty param, but producer gives emptyable
      ParamEmptinessIncompatible
    | -- | Consumer requires mandatory parm, but producer optional
      ParamRequired
    | ParamPlaceIncompatible
    | -- | Params encoded in different styles
      ParamStyleMismatch
    | -- | One of schemas not presented
      ParamSchemaMismatch
    | PathFragmentsDontMatch (ProdCons Text)
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    ParamSchemaMismatch -> ProbablyIssue -- the schema could be catch-all (?)
    _ -> CertainIssue
  describeIssue _ ParamNameMismatch = para "The path fragments don't match."
  describeIssue Forward ParamEmptinessIncompatible = para "The parameter can no longer be empty."
  describeIssue Backward ParamEmptinessIncompatible = para "The parameter can now be empty."
  describeIssue Forward ParamRequired = para "Parameter has become required."
  describeIssue Backward ParamRequired = para "Parameter is no longer required."
  describeIssue _ ParamPlaceIncompatible = para "Parameters in incompatible locations."
  describeIssue _ ParamStyleMismatch = para "Different parameter styles (encodings)."
  describeIssue _ ParamSchemaMismatch = para "Expected a schema, but didn't find one."
  describeIssue ori (PathFragmentsDontMatch (orientProdCons ori -> ProdCons e a)) =
    para $ "Parameter changed from " <> code e <> " to " <> code a <> "."

instance Behavable 'PathFragmentLevel 'SchemaLevel where
  data Behave 'PathFragmentLevel 'SchemaLevel
    = InParamSchema
    deriving stock (Eq, Ord, Show)

  describeBehavior InParamSchema = "JSON Schema"

instance Subtree Param where
  type SubtreeLevel Param = 'PathFragmentLevel
  type CheckEnv Param = '[ProdCons (Traced (Definitions Schema))]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _paramName <$> pc
    structuralEq $ fmap _paramRequired <$> pc
    structuralEq $ fmap _paramIn <$> pc
    structuralEq $ fmap _paramAllowEmptyValue <$> pc
    structuralEq $ fmap _paramAllowReserved <$> pc
    structuralMaybe env $ tracedSchema <$> pc
    structuralEq $ fmap _paramStyle <$> pc
    structuralEq $ fmap _paramExplode <$> pc
    pure ()
  checkSemanticCompatibility env beh pc@(ProdCons p c) = do
    when (_paramName (extract p) /= _paramName (extract c)) $
      issueAt beh ParamNameMismatch
    when
      ( (fromMaybe False . _paramRequired . extract $ c)
          && not (fromMaybe False . _paramRequired . extract $ p)
      )
      $ issueAt beh ParamRequired
    case (_paramIn . extract $ p, _paramIn . extract $ c) of
      (ParamQuery, ParamQuery) -> do
        -- Emptiness is only for query params
        when
          ( (fromMaybe False . _paramAllowEmptyValue . extract $ p)
              && not (fromMaybe False . _paramAllowEmptyValue . extract $ c)
          )
          $ issueAt beh ParamEmptinessIncompatible
      (a, b) | a == b -> pure ()
      _ -> issueAt beh ParamPlaceIncompatible
    unless (paramEncoding (extract p) == paramEncoding (extract c)) $
      issueAt beh ParamStyleMismatch
    case tracedSchema <$> pc of
      ProdCons (Just prodSchema) (Just consSchema) -> do
        checkCompatibility (beh >>> step InParamSchema) env $ ProdCons prodSchema consSchema
      ProdCons Nothing Nothing -> pure ()
      ProdCons Nothing (Just _consSchema) -> issueAt beh ParamSchemaMismatch
      ProdCons (Just _prodSchema) Nothing -> pure ()
    -- If consumer doesn't care then why we should?
    pure ()

instance Steppable Param (Referenced Schema) where
  data Step Param (Referenced Schema) = ParamSchema
    deriving stock (Eq, Ord, Show)
