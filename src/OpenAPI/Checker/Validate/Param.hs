{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OpenAPI.Checker.Validate.Param
  ( CheckIssue (..)
  ) where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.OpenApi
import Data.Text
import OpenAPI.Checker.Orphans
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
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

tracedSchema :: Traced r Param -> Maybe (Traced r (Referenced Schema))
tracedSchema par = _paramSchema (extract par) <&> traced (ask par >>> step ParamSchema)

instance Subtree Param where
  type CheckEnv Param = '[ProdCons (Definitions Schema)]
  data CheckIssue Param
    = ParamNotMatched Text
    | ParamNameMismatch
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
    deriving (Eq, Ord, Show)
  checkCompatibility env pc@(ProdCons p c) = do
    when (_paramName (extract p) /= _paramName (extract c))
      $ issueAt p ParamNameMismatch
    when ((fromMaybe False . _paramRequired . extract $ c) &&
          not (fromMaybe False . _paramRequired . extract $ p))
      $ issueAt p ParamRequired
    case (_paramIn . extract $ p, _paramIn . extract $ c) of
      (ParamQuery, ParamQuery) -> do
        -- Emptiness is only for query params
        when ((fromMaybe False . _paramAllowEmptyValue . extract $ p)
              && not (fromMaybe False . _paramAllowEmptyValue . extract $ c))
          $ issueAt p ParamEmptinessIncompatible
      (a, b) | a == b -> pure ()
      _ -> issueAt p ParamPlaceIncompatible
    unless (paramEncoding (extract p) == paramEncoding (extract c))
      $ issueAt p ParamStyleMismatch
    case tracedSchema <$> pc of
      ProdCons (Just prodSchema) (Just consSchema) -> do
        checkCompatibility env $ ProdCons prodSchema consSchema
      ProdCons Nothing Nothing -> pure ()
      ProdCons Nothing (Just _consSchema) -> issueAt p ParamSchemaMismatch
      ProdCons (Just _prodSchema) Nothing -> pure ()
      -- If consumer doesn't care then why we should?
    pure ()

instance Steppable Param (Referenced Schema) where
  data Step Param (Referenced Schema) = ParamSchema
    deriving (Eq, Ord, Show)
