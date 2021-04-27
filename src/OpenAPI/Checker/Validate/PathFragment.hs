module OpenAPI.Checker.Validate.PathFragment
  ( parsePath
  , PathFragment (..)
  , PathFragmentParam
  )
where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.HList
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import OpenAPI.Checker.References
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

instance Subtree PathFragmentParam where
  type CheckEnv PathFragmentParam =
    '[ ProdCons (Definitions Schema) ]
  data CheckIssue PathFragmentParam =
    PathFragmentsDontMatch Text Text
    deriving (Eq, Ord, Show)
  -- This case isn't strictly needed. It is here for optimization.
  checkCompatibility _ ProdCons {producer = (StaticPath x), consumer = (StaticPath y)} =
    if x == y
      then pure ()
      else issueAt consumer (PathFragmentsDontMatch x y)
  checkCompatibility env prodCons = withTrace $ \myTrace -> do
    let
      tracedParams = dePathFragment <$> myTrace <*> prodCons
      dePathFragment root = \case
        StaticPath s -> retrace root $ Traced (step $ StaticPathParam s)
          $ mempty
          { _paramRequired = Just True
          , _paramIn = ParamPath
          , _paramAllowEmptyValue = Just False
          , _paramAllowReserved = Just False
          , _paramSchema = Just $ Inline $ staticStringSchema s }
        DynamicPath p -> p
      params = getTraced <$> tracedParams
      paramTrace = getTrace <$> tracedParams
    localTrace' paramTrace $ checkCompatibility env params

staticStringSchema :: Text -> Schema
staticStringSchema t =
  mempty
    { _schemaNullable = Just False
    , _schemaType = Just OpenApiString
    , _schemaEnum = Just [A.String t]
    }
