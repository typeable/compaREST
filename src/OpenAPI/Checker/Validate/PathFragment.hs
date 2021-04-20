module OpenAPI.Checker.Validate.PathFragment
  ( parsePath
  , PathFragment (..)
  , PathFragmentParam
  , CheckIssue (..)
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Param ()

-- TODO: templates can be only part of the PathFragment. Currently only supports templates as full PathFragment.
-- https://github.com/typeable/openapi-diff/issues/23
parsePath :: FilePath -> [PathFragment Text]
parsePath = fmap partition . T.splitOn "/" . T.pack
  where
    partition :: Text -> PathFragment Text
    partition t
      | Just ('{', rest) <- T.uncons t
        , Just (ref, '}') <- T.unsnoc rest =
        DynamicPath ref
    partition t = StaticPath t

-- | Fragment parameterized by parameter. The dynamic part may be either
-- reference to some parameter (in context of operation) or dereferenced
-- parameter itself.
data PathFragment param
  = StaticPath Text
  | DynamicPath param
  deriving stock (Eq, Ord)

type PathFragmentParam = PathFragment (Traced OpenApi Param)

instance (Typeable param) => Steppable (PathFragment param) Param where
  data Step (PathFragment param) Param = StaticPathParam Text
    deriving (Eq, Ord, Show)

tracedDePathFragment :: Traced OpenApi PathFragmentParam -> Traced OpenApi Param
tracedDePathFragment pfp = case extract pfp of
  StaticPath s -> traced (ask pfp >>> step (StaticPathParam s))
    $ mempty
    { _paramRequired = Just True
    , _paramIn = ParamPath
    , _paramAllowEmptyValue = Just False
    , _paramAllowReserved = Just False
    , _paramSchema = Just $ Inline $ staticStringSchema s }
  DynamicPath p -> p

staticStringSchema :: Text -> Schema
staticStringSchema t =
  mempty
    { _schemaNullable = Just False
    , _schemaType = Just OpenApiString
    , _schemaEnum = Just [A.String t]
    }

instance Subtree PathFragmentParam where
  type CheckEnv PathFragmentParam =
    '[ ProdCons (Definitions Schema) ]
  data CheckIssue PathFragmentParam
    = PathFragmentNotMatched
    | PathFragmentsDontMatch Text Text
    deriving (Eq, Ord, Show)
  -- This case isn't strictly needed. It is here for optimization.
  checkCompatibility _ (ProdCons (extract -> StaticPath x) c@(extract -> StaticPath y))
    = if x == y
      then pure ()
      else issueAt c (PathFragmentsDontMatch x y)
  checkCompatibility env prodCons = do
    checkCompatibility env (tracedDePathFragment <$> prodCons)
