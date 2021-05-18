{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OpenAPI.Checker.Validate.Param
  ( Behave (..)
  , Issue (..)
  ) where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.OpenApi
import Data.Text as T
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Schema ()

-- | The type is normalized encoding style of the parameter. If two encoding
-- styles are equal then parameters are compatible with their encoding style
data EncodingStyle = EncodingStyle
  { style :: Style
  , explode :: Bool
  , allowReserved :: Maybe Bool
  -- ^ Nothing when @in@ parameter is not @query@
  } deriving (Eq, Ord, Show)

paramEncoding :: Param -> EncodingStyle
paramEncoding p = EncodingStyle
    { style, explode, allowReserved }
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
    = ParamNameMismatch
    -- ^ Params have different names
    | ParamEmptinessIncompatible
    -- ^ Consumer requires non-empty param, but producer gives emptyable
    | ParamRequired
    -- ^ Consumer requires mandatory parm, but producer optional
    | ParamPlaceIncompatible
    | ParamStyleMismatch
    -- ^ Params encoded in different styles
    | ParamSchemaMismatch
    -- ^ One of schemas not presented
    | PathFragmentsDontMatch Text Text
    deriving (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Behavable 'PathFragmentLevel 'SchemaLevel where
  data Behave 'PathFragmentLevel 'SchemaLevel
    = InParamSchema
    deriving (Eq, Ord, Show)

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
    when (_paramName (extract p) /= _paramName (extract c))
      $ issueAt beh ParamNameMismatch
    when ((fromMaybe False . _paramRequired . extract $ c) &&
          not (fromMaybe False . _paramRequired . extract $ p))
      $ issueAt beh ParamRequired
    case (_paramIn . extract $ p, _paramIn . extract $ c) of
      (ParamQuery, ParamQuery) -> do
        -- Emptiness is only for query params
        when ((fromMaybe False . _paramAllowEmptyValue . extract $ p)
              && not (fromMaybe False . _paramAllowEmptyValue . extract $ c))
          $ issueAt beh ParamEmptinessIncompatible
      (a, b) | a == b -> pure ()
      _ -> issueAt beh ParamPlaceIncompatible
    unless (paramEncoding (extract p) == paramEncoding (extract c))
      $ issueAt beh ParamStyleMismatch
    case tracedSchema <$> pc of
      ProdCons (Just prodSchema) (Just consSchema) -> do
        checkCompatibility env (beh >>> step InParamSchema) $ ProdCons prodSchema consSchema
      ProdCons Nothing Nothing -> pure ()
      ProdCons Nothing (Just _consSchema) -> issueAt beh ParamSchemaMismatch
      ProdCons (Just _prodSchema) Nothing -> pure ()
      -- If consumer doesn't care then why we should?
    pure ()

instance Steppable Param (Referenced Schema) where
  data Step Param (Referenced Schema) = ParamSchema
    deriving (Eq, Ord, Show)
