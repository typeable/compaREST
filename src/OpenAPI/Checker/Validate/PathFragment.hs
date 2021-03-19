module OpenAPI.Checker.Validate.PathFragment
  ( PathParamRefs
  , TracedReferences
  , getPathParamRefs
  , parsePath
  , PathFragment (..)
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
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Param ()

getPathParamRefs
  :: Definitions Param
  -> [Referenced Param]
  -> Map Reference (Traced (Referenced Param) Param)
getPathParamRefs defs xs =
  M.fromList $ do
    x <- xs
    let (Traced t param) = dereferenceTraced defs x
    guard (_paramIn param == ParamPath)
    return (Reference $ _paramName param, Traced t param)

-- TODO: templates can be only part of the PathFragment. Currently only supports templates as full PathFragment.
-- https://github.com/typeable/openapi-diff/issues/23
parsePath :: FilePath -> [PathFragment]
parsePath = fmap partition . T.splitOn "/" . T.pack
  where
    partition :: Text -> PathFragment
    partition t
      | Just ('{', rest) <- T.uncons t
        , Just (ref, '}') <- T.unsnoc rest =
        DynamicPath $ Reference ref
    partition t = StaticPath t

data PathFragment
  = StaticPath Text
  | DynamicPath Reference
  deriving stock (Eq, Ord)

instance Steppable PathFragment Param where
  data Step PathFragment Param = StaticPathParam
    deriving (Eq, Ord, Show)

type PathParamRefs = TracedReferences PathFragment Param

instance Subtree PathFragment where
  type CheckEnv PathFragment = '[ProdCons PathParamRefs]
  data CheckIssue PathFragment = PathFragmentsDontMatch Text Text
    deriving (Eq, Ord, Show)

  normalizeTrace = undefined

  -- This case isn't strictly needed. It is here for optimization.
  checkCompatibility _ ProdCons {producer = (StaticPath x), consumer = (StaticPath y)} =
    if x == y
      then pure ()
      else issueAt consumer (PathFragmentsDontMatch x y)
  checkCompatibility env prodCons = do
    let (t, param) =
          fsplit . fmap deTraced $
            dePathFragment
              <$> (singletonH <$> getH @(ProdCons PathParamRefs) env)
              <*> prodCons
    localTrace t $ checkCompatibility env param

-- | A clearer name for 'NE.unzip' that can be used without qualifying it.
fsplit :: Functor f => f (a, b) -> (f a, f b)
fsplit = NE.unzip

dePathFragment :: Has PathParamRefs xs => HList xs -> PathFragment -> Traced PathFragment Param
dePathFragment (getH @PathParamRefs -> params) = \case
  (StaticPath s) ->
    Traced (step StaticPathParam) $
      mempty
        { _paramRequired = Just True
        , _paramIn = ParamPath
        , _paramAllowEmptyValue = Just False
        , _paramAllowReserved = Just False
        , _paramSchema = Just $ Inline $ staticStringSchema s
        }
  (DynamicPath ref) -> M.lookup ref params & fromMaybe (error $ show ref <> " not found.")

staticStringSchema :: Text -> Schema
staticStringSchema t =
  mempty
    { _schemaNullable = Just False
    , _schemaType = Just OpenApiString
    , _schemaEnum = Just [A.String t]
    }
