{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenAPI.Checker.Validate.Schema
  ( JsonType (..)
  , ForeachType (..)
  , TypedValue (..)
  , Bound (..)
  , schemaToFormula
  , foldLattice
  )
  where

import Algebra.Lattice
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Aeson as A
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Kind
import qualified Data.Map as M
import Data.OpenApi
import Data.Ord
import Data.Scientific
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

data JsonType
  = Null
  | Boolean
  | Number
  | String
  | Array
  | Object
  deriving (Eq, Show)

data ForeachType (f :: JsonType -> Type) = ForeachType
  { forNull :: f 'Null
  , forBoolean :: f 'Boolean
  , forNumber :: f 'Number
  , forString :: f 'String
  , forArray :: f 'Array
  , forObject :: f 'Object
  }

data TypedValue :: JsonType -> Type where
  TNull :: TypedValue 'Null
  TBool :: !Bool -> TypedValue 'Boolean
  TNumber :: !Scientific -> TypedValue 'Number
  TString :: !Text -> TypedValue 'String
  TArray :: !A.Array -> TypedValue 'Array
  TObject :: !A.Object -> TypedValue 'Object

deriving stock instance Eq (TypedValue t)
deriving stock instance Ord (TypedValue t)

data Bound a = Exclusive !a | Inclusive !a
  deriving (Eq, Show)

-- | The order is lexicographical on @a * Bool@.
instance Ord a => Ord (Bound a) where
  Exclusive a `compare` Exclusive b = compare a b
  Exclusive a `compare` Inclusive b = if a <= b then LT else GT
  Inclusive a `compare` Exclusive b = if a < b then LT else GT
  Inclusive a `compare` Inclusive b = compare a b

data Condition :: JsonType -> Type where
  Exactly :: TypedValue t -> Condition t
  Maximum :: !(Bound Scientific) -> Condition 'Number
  Minimum :: !(Down (Bound Scientific)) -> Condition 'Number
  MultipleOf :: !Scientific -> Condition 'Number
  NumberFormat :: !Format -> Condition 'Number
  MaxLength :: !Integer -> Condition 'String
  MinLength :: !Integer -> Condition 'String
  Pattern :: !Pattern -> Condition 'String
  StringFormat :: !Format -> Condition 'String
  Items :: !(Referenced Schema) -> Condition 'Array
  MaxItems :: !Integer -> Condition 'Array
  MinItems :: !Integer -> Condition 'Array
  UniqueItems :: Condition 'Array
  Properties
    :: !(IOHM.InsOrdHashMap Text (Bool, Referenced Schema))
    -> !(Maybe (Referenced Schema)) -- additional properties. Nothing means forbidden
    -> Condition 'Object
  MaxProperties :: !Integer -> Condition 'Object
  MinProperties :: !Integer -> Condition 'Object

deriving stock instance Eq (Condition t)
deriving stock instance Ord (Condition t)

newtype JsonFormula r t
  = DNF (S.Set (M.Map (Condition t) (Trace r (Condition t))))

disjAdd
  :: JsonFormula r t
  -> M.Map (Condition t) (Trace r (Condition t))
  -> JsonFormula r t
disjAdd (DNF yss) xs
  | any (`isMapSubsetOf` xs) yss = DNF yss
  | otherwise = DNF $ S.insert xs $ S.filter (not . isMapSubsetOf xs) yss
  where
    isMapSubsetOf a b = M.keysSet a `S.isSubsetOf` M.keysSet b

instance Lattice (JsonFormula r t) where
  xss \/ DNF yss = S.foldl' disjAdd xss yss
  DNF xss /\ DNF yss = F.foldl' disjAdd bottom $
    liftA2 M.union (S.toList xss) (S.toList yss)

instance BoundedJoinSemiLattice (JsonFormula r t) where
  bottom = DNF S.empty

instance BoundedMeetSemiLattice (JsonFormula r t) where
  top = DNF $ S.singleton M.empty

singletonFormula :: Trace r (Condition t) -> Condition t -> JsonFormula r t
singletonFormula t x = DNF $ S.singleton $ M.singleton x t

foldLattice
  :: BoundedLattice l
  => (Traced r (Condition t) -> l)
  -> JsonFormula r t
  -> l
foldLattice f (DNF xss) = S.foldl' (\z w ->
  z \/ M.foldlWithKey' (\x y t -> x /\ f (Traced t y)) top w) bottom xss

instance (forall x. Lattice (f x)) => Lattice (ForeachType f) where
  f1 \/ f2 = ForeachType
    { forNull = forNull f1 \/ forNull f2
    , forBoolean = forBoolean f1 \/ forBoolean f2
    , forNumber = forNumber f1 \/ forNumber f2
    , forString = forString f1 \/ forString f2
    , forArray = forArray f1 \/ forArray f2
    , forObject = forObject f1 \/ forObject f2
    }
  f1 /\ f2 = ForeachType
    { forNull = forNull f1 /\ forNull f2
    , forBoolean = forBoolean f1 /\ forBoolean f2
    , forNumber = forNumber f1 /\ forNumber f2
    , forString = forString f1 /\ forString f2
    , forArray = forArray f1 /\ forArray f2
    , forObject = forObject f1 /\ forObject f2
    }

instance (forall x. BoundedJoinSemiLattice (f x))
  => BoundedJoinSemiLattice (ForeachType f) where
  bottom = ForeachType
    { forNull = bottom
    , forBoolean = bottom
    , forNumber = bottom
    , forString = bottom
    , forArray = bottom
    , forObject = bottom
    }

instance (forall x. BoundedMeetSemiLattice (f x))
  => BoundedMeetSemiLattice (ForeachType f) where
  top = ForeachType
    { forNull = top
    , forBoolean = top
    , forNumber = top
    , forString = top
    , forArray = top
    , forObject = top
    }

instance Typeable t => Steppable Schema (Condition t) where
  data Step Schema (Condition t)
    = EnumField
    | MaximumField
    | MinimumField
    | MultipleOfField
    | FormatField
    | MaxLengthField
    | MinLengthField
    | PatternField
    | ItemsField
    | MaxItemsField
    | MinItemsField
    | UniqueItemsField
    | PropertiesField
    | MaxPropertiesField
    | MinPropertiesField
    | NullableField
    | TypeField
    deriving (Eq, Ord, Show)

instance Steppable Schema (Referenced Schema) where
  data Step Schema (Referenced Schema)
    = AllOfField Int
    | OneOfField Int
    | AnyOfField Int
    deriving (Eq, Ord, Show)

type ProcessM = ReaderT (Definitions Schema) (Writer (T.TracePrefixTree SubtreeCheckIssue OpenApi))

warn :: Subtree t => Trace OpenApi t -> CheckIssue t -> ProcessM ()
warn t x = tell $ T.singleton $ AnItem t $ SubtreeCheckIssue x

processRefSchema
  :: Traced OpenApi (Referenced Schema)
  -> ProcessM (ForeachType (JsonFormula OpenApi))
processRefSchema (Traced t x) = do
  defs <- ask
  processSchema $ retrace (t >>>) $ dereferenceTraced defs x

processSchema
  :: Traced OpenApi Schema
  -> ProcessM (ForeachType (JsonFormula OpenApi))
processSchema (Traced t Schema{..}) = do

  allClauses <- case _schemaAllOf of
    Nothing -> pure []
    Just [] -> [] <$ warn t (InvalidSchema "no items in allOf")
    Just xs -> sequence
      [ processRefSchema (Traced (t `Snoc` AllOfField i) rs)
      | (i, rs) <- zip [0..] xs ]

  anyClause <- case _schemaAnyOf of
    Nothing -> pure top
    Just [] -> bottom <$ warn t (InvalidSchema "no items in anyOf")
    Just xs -> joins <$> sequence
      [ processRefSchema (Traced (t `Snoc` AnyOfField i) rs)
      | (i, rs) <- zip [0..] xs ]

  oneClause <- case _schemaOneOf of
    Nothing -> pure top
    Just [] -> bottom <$ warn t (InvalidSchema "no items in oneOf")
    Just xs -> do
      checkOneOfDisjoint xs >>= \case
        True -> pure ()
        False -> warn t (NotSupported "Could not determine that oneOf branches are disjoint")
      joins <$> sequence
        [ processRefSchema (Traced (t `Snoc` OneOfField i) rs)
        | (i, rs) <- zip [0..] xs ]

  case _schemaNot of
    Nothing -> pure ()
    Just _ -> warn t (NotSupported "not clause is unsupported")

  let
    valueEnum A.Null = bottom
      { forNull = singletonFormula (t `Snoc` EnumField) $ Exactly TNull }
    valueEnum (A.Bool b) = bottom
      { forBoolean = singletonFormula (t `Snoc` EnumField) $ Exactly $ TBool b }
    valueEnum (A.Number n) = bottom
      { forNumber = singletonFormula (t `Snoc` EnumField) $ Exactly $ TNumber n }
    valueEnum (A.String s) = bottom
      { forString = singletonFormula (t `Snoc` EnumField) $ Exactly $ TString s }
    valueEnum (A.Array a) = bottom
      { forArray = singletonFormula (t `Snoc` EnumField) $ Exactly $ TArray a }
    valueEnum (A.Object o) = bottom
      { forObject = singletonFormula (t `Snoc` EnumField) $ Exactly $ TObject o }
  enumClause <- case _schemaEnum of
    Nothing -> pure top
    Just [] -> bottom <$ warn t (InvalidSchema "no items in enum")
    Just xs -> pure $ joins (valueEnum <$> xs)

  let
    maximumClause = case _schemaMaximum of
      Nothing -> top
      Just n -> top
        { forNumber = singletonFormula (t `Snoc` MaximumField) $ Maximum $
          case _schemaExclusiveMaximum of
            Just True -> Exclusive n
            _ -> Inclusive n }

    minimumClause = case _schemaMinimum of
      Nothing -> top
      Just n -> top
        { forNumber = singletonFormula (t `Snoc` MinimumField) $ Minimum $ Down $
          case _schemaExclusiveMinimum of
            Just True -> Exclusive n
            _ -> Inclusive n }

    multipleOfClause = case _schemaMultipleOf of
      Nothing -> top
      Just n -> top
        { forNumber = singletonFormula (t `Snoc` MultipleOfField) $ MultipleOf n }

  formatClause <- case _schemaFormat of
    Nothing -> pure top
    Just f | f `elem` ["int32", "int64", "float", "double"] -> pure top
      { forNumber = singletonFormula (t `Snoc` FormatField) $ NumberFormat f }
    Just f | f `elem` ["byte", "binary", "date", "date-time", "password"] -> pure top
      { forString = singletonFormula (t `Snoc` FormatField) $ StringFormat f }
    Just f -> top <$ warn t (NotSupported $ "Unknown format: " <> f)

  let
    maxLengthClause = case _schemaMaxLength of
      Nothing -> top
      Just n -> top
        { forString = singletonFormula (t `Snoc` MaxLengthField) $ MaxLength n }

    minLengthClause = case _schemaMinLength of
      Nothing -> top
      Just n -> top
        { forString = singletonFormula (t `Snoc` MinLengthField) $ MinLength n }

    patternClause = case _schemaPattern of
      Nothing -> top
      Just p -> top
        { forString = singletonFormula (t `Snoc` PatternField) $ Pattern p }

  itemsClause <- case _schemaItems of
    Nothing -> pure top
    Just (OpenApiItemsObject rs) -> pure top
      { forArray = singletonFormula (t `Snoc` ItemsField) $ Items rs }
    Just (OpenApiItemsArray _) -> top <$ warn t (NotSupported "array in items is not supported")

  let
    maxItemsClause = case _schemaMaxItems of
      Nothing -> top
      Just n -> top
        { forArray = singletonFormula (t `Snoc` MaxItemsField) $ MaxItems n }

    minItemsClause = case _schemaMinItems of
      Nothing -> top
      Just n -> top
        { forArray = singletonFormula (t `Snoc` MinItemsField) $ MinItems n }

    uniqueItemsClause = case _schemaUniqueItems of
      Just True -> top
        { forArray = singletonFormula (t `Snoc` UniqueItemsField) UniqueItems }
      _ -> top

  let
    additionalProps = case _schemaAdditionalProperties of
      Just (AdditionalPropertiesSchema rs) -> Just rs
      Just (AdditionalPropertiesAllowed False) -> Nothing
      _ -> Just $ Inline mempty
    requiredAdditional = forM _schemaRequired $ \k -> do
      addProp <- additionalProps
      pure (k, (False, addProp))
    props = IOHM.mapWithKey (\k x -> (k `elem` _schemaRequired, x)) _schemaProperties
    propertiesClause = case requiredAdditional of
      Nothing -> bottom
      Just addProps -> case props `IOHM.union` IOHM.fromList addProps of
        hm | IOHM.null hm -> top
        hm -> top
          { forObject = singletonFormula (t `Snoc` PropertiesField) $
            Properties hm additionalProps }

    maxPropertiesClause = case _schemaMaxProperties of
      Nothing -> top
      Just n -> top
        { forObject = singletonFormula (t `Snoc` MaxPropertiesField) $ MaxProperties n }

    minPropertiesClause = case _schemaMinProperties of
      Nothing -> top
      Just n -> top
        { forObject = singletonFormula (t `Snoc` MinPropertiesField) $ MinProperties n }

    nullableClause
      | Just True <- _schemaNullable = bottom
        { forNull = singletonFormula (t `Snoc` NullableField) $ Exactly TNull }
      | otherwise = bottom

  pure $ nullableClause \/ meets (allClauses <>
    [ anyClause, oneClause, enumClause, maximumClause, minimumClause
    , multipleOfClause, formatClause, maxLengthClause, minLengthClause
    , patternClause, itemsClause, maxItemsClause, minItemsClause
    , uniqueItemsClause, propertiesClause, maxPropertiesClause, minPropertiesClause])
{- TODO: ReadOnly/WriteOnly -}

checkOneOfDisjoint :: [Referenced Schema] -> ProcessM Bool
checkOneOfDisjoint = undefined

schemaToFormula
  :: Traced OpenApi (Referenced Schema)
  -> Definitions Schema
  -> (ForeachType (JsonFormula OpenApi), T.TracePrefixTree SubtreeCheckIssue OpenApi)
schemaToFormula rs defs = runWriter . (`runReaderT` defs) $ processRefSchema rs

instance Subtree Schema where
  data CheckIssue Schema
    = NotSupported Text
    | InvalidSchema Text
    deriving (Eq, Ord, Show)
  type CheckEnv Schema = '[]
  normalizeTrace = undefined
  checkCompatibility = undefined
