{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OpenAPI.Checker.Validate.Param () where

import Control.Monad
import Data.Maybe
import Data.OpenApi
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

instance Subtree Param where
  type CheckEnv Param = '[ProdCons (Definitions Schema)]
  data CheckIssue Param
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
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    when (_paramName p /= _paramName c)
      $ issueAt producer ParamNameMismatch
    when ((fromMaybe False $ _paramRequired c) &&
          not (fromMaybe False $ _paramRequired p))
      $ issueAt producer ParamRequired
    case (_paramIn p, _paramIn c) of
      (ParamQuery, ParamQuery) -> do
        -- Emptiness is only for query params
        when ((fromMaybe False $ _paramAllowEmptyValue p)
              && not (fromMaybe False $ _paramAllowEmptyValue c))
          $ issueAt producer ParamEmptinessIncompatible
      (a, b) | a == b -> pure ()
      _ -> issueAt producer ParamPlaceIncompatible
    unless (paramEncoding p == paramEncoding c)
      $ issueAt producer ParamStyleMismatch
    case (_paramSchema p, _paramSchema c) of
      (Just prodSchema, Just consSchema) -> localStep ParamSchema
        $ checkCompatibility env (ProdCons prodSchema consSchema)
      (Nothing, Nothing) -> pure ()
      (Nothing, Just _consSchema) -> issueAt producer ParamSchemaMismatch
      (Just _prodSchema, Nothing) -> pure ()
      -- If consumer doesn't care then why we should?
    pure ()

instance Steppable Param (Referenced Schema) where
  data Step Param (Referenced Schema) = ParamSchema
    deriving (Eq, Ord, Show)
