module OpenAPI.Checker.Validate.PathFragment
  ( PathParamRefs,
    TracedReferences,
    getPathParamRefs,
    parsePath,
    PathFragment (..),
  )
where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace

instance Typeable a => Steppable (Referenced a) Param where
  data Step (Referenced a) Param
    = InlineStep
    | ReferencedStep Reference
    deriving (Eq, Ord)

dereferenceTraced :: (Reference -> Trace x a) -> Trace x a -> Definitions a -> Referenced a -> (Trace x a, a)
dereferenceTraced _ inl _ (Inline a) = (inl, a)
dereferenceTraced refed _ defs (Ref r@(Reference ref)) = (refed r, fromJust $ IOHM.lookup ref defs)

dereferenceStepped ::
  Steppable x a =>
  (Reference -> Step x a) ->
  Step x a ->
  Definitions a ->
  Referenced a ->
  (Trace x a, a)
dereferenceStepped r i = dereferenceTraced (step . r) (step i)

getPathParamRefs ::
  Has (Definitions Param) xs =>
  HList xs ->
  [Referenced Param] ->
  Map Reference (Traced (Referenced Param) Param)
getPathParamRefs (getH -> defs) xs =
  M.fromList $ do
    x <- xs
    let (t, param) = dereferenceStepped ReferencedStep InlineStep defs x
    guard (_paramIn param == ParamPath)
    return (Reference $ _paramName param, Traced t param)

-- TODO: templates can be only part of the PathFragment. Currently only supports templates as full PathFragment.
parsePath :: FilePath -> [PathFragment]
parsePath = fmap partition . T.splitOn "/" . T.pack
  where
    partition :: Text -> PathFragment
    partition t
      | Just ('{', rest) <- T.uncons t,
        Just (ref, '}') <- T.unsnoc rest =
        DynamicPath $ Reference ref
    partition t = StaticPath t

data PathFragment
  = StaticPath Text
  | DynamicPath Reference
  deriving stock (Eq, Ord)

instance Steppable PathFragment Param where
  data Step PathFragment Param = StaticPathParam
    deriving (Eq, Ord)

-- instance Steppable PathFragment PathItem where
--   data Step PathFragment PathItem = XXX
--     deriving (Eq, Ord)

type TracedReferences root a = Map Reference (Traced root a)

type PathParamRefs = TracedReferences PathFragment Param

instance Subtree PathFragment where
  type CheckEnv PathFragment = '[ProdCons PathParamRefs]
  data CheckIssue PathFragment = PathFragmentsDontMatch Text Text
    deriving (Eq, Ord)

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

fsplit :: Functor f => f (a, b) -> (f a, f b)
fsplit xs = (fst <$> xs, snd <$> xs)

instance Subtree Param where
  type CheckEnv Param = '[]
  data CheckIssue Param
    deriving (Eq, Ord)

-- f (a -> b) -> g a -> f (g b)

-- u <- (fmap . fmap) (<$> prodCons) $ liftProdCons dePathFragment
-- u <- liftA2 (<*>) (liftProdCons dePathFragment) (pure prodCons)
-- x <- liftProdCons dePathFragment <*> pure prodCons
-- let foo = f <*> prodCons
-- pure ()

dePathFragment :: Has PathParamRefs xs => HList xs -> PathFragment -> Traced PathFragment Param
dePathFragment (getH @PathParamRefs -> params) = \case
  (StaticPath s) ->
    Traced (step StaticPathParam) $
      Param
        { _paramName = "", -- Text
          _paramDescription = Nothing, -- Maybe Text
          _paramRequired = Just True, -- Maybe Bool
          _paramDeprecated = Nothing, -- Maybe Bool
          _paramIn = ParamPath, -- ParamLocation
          _paramAllowEmptyValue = Just False, -- Maybe Bool
          _paramAllowReserved = Just False, -- Maybe Bool
          _paramSchema = Just $ Inline $ staticStringSchema s, -- Maybe (Referenced Schema)
          _paramStyle = Nothing, -- Maybe Style
          _paramExplode = Nothing, -- Maybe Bool
          _paramExample = Nothing, -- Maybe Value
          _paramExamples = mempty -- InsOrdHashMap Text (Referenced Example)
        }
  (DynamicPath ref) -> M.lookup ref params & fromMaybe (error $ show ref <> " not found.")

staticStringSchema :: Text -> Schema
staticStringSchema t =
  Schema
    { _schemaTitle = Nothing, -- Maybe Text
      _schemaDescription = Nothing, -- Maybe Text
      _schemaRequired = [], -- [ParamName]
      _schemaNullable = Just False, -- Maybe Bool
      _schemaAllOf = Nothing, -- Maybe [Referenced Schema]
      _schemaOneOf = Nothing, -- Maybe [Referenced Schema]
      _schemaNot = Nothing, -- Maybe (Referenced Schema)
      _schemaAnyOf = Nothing, -- Maybe [Referenced Schema]
      _schemaProperties = mempty, -- InsOrdHashMap Text (Referenced Schema)
      _schemaAdditionalProperties = Nothing, -- Maybe AdditionalProperties
      _schemaDiscriminator = Nothing, -- Maybe Discriminator
      _schemaReadOnly = Nothing, -- Maybe Bool
      _schemaWriteOnly = Nothing, -- Maybe Bool
      _schemaXml = Nothing, -- Maybe Xml
      _schemaExternalDocs = Nothing, -- Maybe ExternalDocs
      _schemaExample = Nothing, -- Maybe Value
      _schemaDeprecated = Nothing, -- Maybe Bool
      _schemaMaxProperties = Nothing, -- Maybe Integer
      _schemaMinProperties = Nothing, -- Maybe Integer
      _schemaDefault = Nothing, -- Maybe Value
      _schemaType = Just OpenApiString, -- Maybe OpenApiType
      _schemaFormat = Nothing, -- Maybe Format
      _schemaItems = Nothing, -- Maybe OpenApiItems
      _schemaMaximum = Nothing, -- Maybe Scientific
      _schemaExclusiveMaximum = Nothing, -- Maybe Bool
      _schemaMinimum = Nothing, -- Maybe Scientific
      _schemaExclusiveMinimum = Nothing, -- Maybe Bool
      _schemaMaxLength = Nothing, -- Maybe Integer
      _schemaMinLength = Nothing, -- Maybe Integer
      _schemaPattern = Nothing, -- Maybe Pattern
      _schemaMaxItems = Nothing, -- Maybe Integer
      _schemaMinItems = Nothing, -- Maybe Integer
      _schemaUniqueItems = Nothing, -- Maybe Bool
      _schemaEnum = Just [A.String t], -- Maybe [Value]
      _schemaMultipleOf = Nothing -- Maybe Scientific
    }
