{-# OPTIONS_GHC -fno-warn-orphans #-}
module OpenAPI.Checker.Validate.Schema
  ( JsonType (..)
  , ForeachType (..)
  , foldType
  , forType_
  , TypedValue (..)
  , untypeValue
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
import Data.Coerce
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.HList
import Data.Int
import Data.Kind
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T hiding (singleton)
import Data.Typeable
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

-- | Type of a JSON value
data JsonType
  = Null
  | Boolean
  | Number
  | String
  | Array
  | Object
  deriving (Eq, Show)

-- | A 'A.Value' whose type we know
data TypedValue :: JsonType -> Type where
  TNull :: TypedValue 'Null
  TBool :: !Bool -> TypedValue 'Boolean
  TNumber :: !Scientific -> TypedValue 'Number
  TString :: !Text -> TypedValue 'String
  TArray :: !A.Array -> TypedValue 'Array
  TObject :: !A.Object -> TypedValue 'Object

deriving stock instance Eq (TypedValue t)
deriving stock instance Ord (TypedValue t)
deriving stock instance Show (TypedValue t)

untypeValue :: TypedValue t -> A.Value
untypeValue TNull = A.Null
untypeValue (TBool b) = A.Bool b
untypeValue (TNumber n) = A.Number n
untypeValue (TString s) = A.String s
untypeValue (TArray a) = A.Array a
untypeValue (TObject o) = A.Object o

data Bound a = Exclusive !a | Inclusive !a
  deriving (Eq, Show)

-- | The order is lexicographical on @a * Bool@.
instance Ord a => Ord (Bound a) where
  Exclusive a `compare` Exclusive b = compare a b
  Exclusive a `compare` Inclusive b = if a <= b then LT else GT
  Inclusive a `compare` Exclusive b = if a < b then LT else GT
  Inclusive a `compare` Inclusive b = compare a b

data Property = Property
  { propRequired :: Bool
  , propFormula :: ForeachType (JsonFormula OpenApi)
  , propRefSchema :: Traced OpenApi (Referenced Schema)
  }
  deriving stock (Eq, Ord, Show)

-- | A primitive structural condition for the "top level" of a JSON value (of a specific type)
data Condition :: JsonType -> Type where
  Exactly :: TypedValue t -> Condition t
  Maximum :: !(Bound Scientific) -> Condition 'Number
  Minimum :: !(Down (Bound (Down Scientific))) -> Condition 'Number
    -- ^ this has the right Ord
  MultipleOf :: !Scientific -> Condition 'Number
  NumberFormat :: !Format -> Condition 'Number
  MaxLength :: !Integer -> Condition 'String
  MinLength :: !Integer -> Condition 'String
  Pattern :: !Pattern -> Condition 'String
  StringFormat :: !Format -> Condition 'String
  Items
    :: !(ForeachType (JsonFormula OpenApi))
    -> !(Traced OpenApi (Referenced Schema))
    -> Condition 'Array
  MaxItems :: !Integer -> Condition 'Array
  MinItems :: !Integer -> Condition 'Array
  UniqueItems :: Condition 'Array
  Properties
    :: !(M.Map Text Property)
    -> !(ForeachType (JsonFormula OpenApi))
      -- ^ formula for additional properties
    -> !(Maybe (Traced OpenApi (Referenced Schema)))
      -- ^ schema for additional properties, Nothing means bottom
    -> Condition 'Object
  MaxProperties :: !Integer -> Condition 'Object
  MinProperties :: !Integer -> Condition 'Object

satisfiesTyped :: TypedValue t -> Condition t -> Bool
satisfiesTyped e (Exactly e') = e == e'
satisfiesTyped (TNumber n) (Maximum (Exclusive m)) = n < m
satisfiesTyped (TNumber n) (Maximum (Inclusive m)) = n <= m
satisfiesTyped (TNumber n) (Minimum (Down (Exclusive (Down m)))) = n > m
satisfiesTyped (TNumber n) (Minimum (Down (Inclusive (Down m)))) = n >= m
satisfiesTyped (TNumber n) (MultipleOf m) = denominator (toRational n / toRational m) == 1 -- TODO: could be better
satisfiesTyped (TNumber n) (NumberFormat f) = checkNumberFormat f n
satisfiesTyped (TString s) (MaxLength m) = fromIntegral (T.length s) <= m
satisfiesTyped (TString s) (MinLength m) = fromIntegral (T.length s) >= m
satisfiesTyped (TString s) (Pattern p) = undefined s p -- TODO: regex stuff
satisfiesTyped (TString s) (StringFormat f) = undefined s f-- TODO: string format
satisfiesTyped (TArray a) (Items f _) = all (`satisfies` f) a
satisfiesTyped (TArray a) (MaxItems m) = fromIntegral (F.length a) <= m
satisfiesTyped (TArray a) (MinItems m) = fromIntegral (F.length a) >= m
satisfiesTyped (TArray a) UniqueItems = S.size (S.fromList $ F.toList a) == F.length a -- TODO: could be better
satisfiesTyped (TObject o) (Properties props additional _)
  = all (`HM.member` o) (M.keys (M.filter propRequired props))
  && all (\(k, v) -> satisfies v $ maybe additional propFormula $ M.lookup k props) (HM.toList o)
satisfiesTyped (TObject o) (MaxProperties m) = fromIntegral (HM.size o) <= m
satisfiesTyped (TObject o) (MinProperties m) = fromIntegral (HM.size o) >= m

checkNumberFormat :: Format -> Scientific -> Bool
checkNumberFormat "int32" (toRational -> n) = denominator n == 1
  && n >= toRational (minBound :: Int32) && n <= toRational (maxBound :: Int32)
checkNumberFormat "int64" (toRational -> n) = denominator n == 1
  && n >= toRational (minBound :: Int64) && n <= toRational (maxBound :: Int64)
checkNumberFormat "float" _n = True
checkNumberFormat "double" _n = True
checkNumberFormat f _n = error $ "Invalid number format: " <> T.unpack f

deriving stock instance Eq (Condition t)
deriving stock instance Ord (Condition t)
deriving stock instance Show (Condition t)

data SomeCondition where
  SomeCondition :: Typeable t => Condition t -> SomeCondition

instance Eq SomeCondition where
  SomeCondition x == SomeCondition y = case cast x of
    Just x' -> x' == y
    Nothing -> False

instance Ord SomeCondition where
  compare (SomeCondition x) (SomeCondition y) = case cast x of
    Just x' -> compare x' y
    Nothing -> compare (typeRep x) (typeRep y)

deriving stock instance Show SomeCondition

-- | A boolean formula (without "not") of 'Condition's. Represented as a
-- Disjunctive Normal Form: the formula is a disjunction of a set of conjuncts,
-- each of which is a conjunction of a set of 'Condition's.
newtype JsonFormula r t
  = DNF (S.Set (M.Map (Condition t) (Trace r (Condition t))))
  deriving stock (Eq, Ord, Show)

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

pattern BottomFormula :: JsonFormula r t
pattern BottomFormula <- DNF (S.null -> True)
  where BottomFormula = DNF S.empty

isSingleton :: S.Set a -> Maybe a
isSingleton s
  | S.size s == 1 = S.lookupMin s
  | otherwise = Nothing

pattern Conjunct :: [Traced r (Condition t)] -> M.Map (Condition t) (Trace r (Condition t))
pattern Conjunct xs <- (map (uncurry $ flip Traced) . M.toList -> xs)
  where Conjunct xs = M.fromList [(x, t) | Traced t x <- xs]
{-# COMPLETE Conjunct #-}

pattern SingleConjunct :: [Traced r (Condition t)] -> JsonFormula r t
pattern SingleConjunct xs <- DNF (isSingleton -> Just (Conjunct xs))
  where SingleConjunct xs = DNF $ S.singleton $ Conjunct xs

pattern TopFormula :: JsonFormula r t
pattern TopFormula <- DNF (isSingleton -> Just (M.null -> True))
  where TopFormula = DNF $ S.singleton M.empty

instance BoundedJoinSemiLattice (JsonFormula r t) where
  bottom = BottomFormula

instance BoundedMeetSemiLattice (JsonFormula r t) where
  top = TopFormula

singletonFormula :: Trace r (Condition t) -> Condition t -> JsonFormula r t
singletonFormula t x = SingleConjunct [Traced t x]

foldLattice
  :: BoundedLattice l
  => (Traced r (Condition t) -> l)
  -> JsonFormula r t
  -> l
foldLattice f (DNF xss) = S.foldl' (\z w ->
  z \/ M.foldlWithKey' (\x y t -> x /\ f (Traced t y)) top w) bottom xss

satisfiesFormula :: TypedValue t -> JsonFormula r t -> Bool
satisfiesFormula val = foldLattice (satisfiesTyped val . getTraced)

data ForeachType (f :: JsonType -> Type) = ForeachType
  { forNull :: f 'Null
  , forBoolean :: f 'Boolean
  , forNumber :: f 'Number
  , forString :: f 'String
  , forArray :: f 'Array
  , forObject :: f 'Object
  }

satisfies :: A.Value -> ForeachType (JsonFormula r) -> Bool
satisfies val p = case val of
  A.Null -> satisfiesFormula TNull $ forNull p
  A.Bool b -> satisfiesFormula (TBool b) $ forBoolean p
  A.Number n -> satisfiesFormula (TNumber n) $ forNumber p
  A.String s -> satisfiesFormula (TString s) $ forString p
  A.Array a -> satisfiesFormula (TArray a) $ forArray p
  A.Object o -> satisfiesFormula (TObject o) $ forObject p

deriving stock instance (forall x. Typeable x => Eq (f x)) => Eq (ForeachType f)
deriving stock instance (forall x. Typeable x => Ord (f x)) => Ord (ForeachType f)
deriving stock instance (forall x. Typeable x => Show (f x)) => Show (ForeachType f)

foldType
  :: Monoid m
  => (forall x. Typeable x => (ForeachType f -> f x) -> m)
  -> m
foldType k =
  k forNull <>
  k forBoolean <>
  k forNumber <>
  k forString <>
  k forArray <>
  k forObject

forType_
  :: Applicative m
  => (forall x. Typeable x => (ForeachType f -> f x) -> m ())
  -> m ()
forType_ k = do
  k forNull
  k forBoolean
  k forNumber
  k forString
  k forArray
  k forObject
  pure ()

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
    | MaximumFields -- maximum & exclusiveMaximum
    | MinimumFields -- minimum & exclusiveMinimum
    | MultipleOfField
    | FormatField
    | MaxLengthField
    | MinLengthField
    | PatternField
    | ItemsField
    | MaxItemsField
    | MinItemsField
    | UniqueItemsField
    | PropertiesFields -- properties, additionalProperties & required
    | MaxPropertiesField
    | MinPropertiesField
    | NullableField
    | IntegerType -- type=integer
    deriving (Eq, Ord, Show)

instance Steppable Schema (Referenced Schema) where
  data Step Schema (Referenced Schema)
    = AllOfStep Int
    | OneOfStep Int
    | AnyOfStep Int
    | ItemsStep
    | AdditionalPropertiesStep
    | PropertiesStep Text
    deriving (Eq, Ord, Show)

type ProcessM = ReaderT (Definitions Schema) (Writer (T.TracePrefixTree SubtreeCheckIssue OpenApi))

warn :: Subtree t => Trace OpenApi t -> CheckIssue t -> ProcessM ()
warn t x = tell $ T.singleton $ AnItem t $ SubtreeCheckIssue x

processRefSchema
  :: Traced OpenApi (Referenced Schema)
  -> ProcessM (ForeachType (JsonFormula OpenApi))
processRefSchema x = do
  defs <- ask
  processSchema $ dereferenceTraced defs x

-- | Turn a schema into a tuple of 'JsonFormula's that describes the condition
-- for every possible type of a JSON value. The conditions are independent, and
-- are thus checked independently.
processSchema
  :: Traced OpenApi Schema
  -> ProcessM (ForeachType (JsonFormula OpenApi))
processSchema (Traced t Schema{..}) = do

  allClauses <- case _schemaAllOf of
    Nothing -> pure []
    Just [] -> [] <$ warn t (InvalidSchema "no items in allOf")
    Just xs -> sequence
      [ processRefSchema (Traced (t `Snoc` AllOfStep i) rs)
      | (i, rs) <- zip [0..] xs ]

  anyClause <- case _schemaAnyOf of
    Nothing -> pure top
    Just [] -> bottom <$ warn t (InvalidSchema "no items in anyOf")
    Just xs -> joins <$> sequence
      [ processRefSchema (Traced (t `Snoc` AnyOfStep i) rs)
      | (i, rs) <- zip [0..] xs ]

  oneClause <- case _schemaOneOf of
    Nothing -> pure top
    Just [] -> bottom <$ warn t (InvalidSchema "no items in oneOf")
    Just xs -> do
      checkOneOfDisjoint xs >>= \case
        True -> pure ()
        False -> warn t (NotSupported "Could not determine that oneOf branches are disjoint")
      joins <$> sequence
        [ processRefSchema (Traced (t `Snoc` OneOfStep i) rs)
        | (i, rs) <- zip [0..] xs ]

  case _schemaNot of
    Nothing -> pure ()
    Just _ -> warn t (NotSupported "not clause is unsupported")

  let
    typeClause = case _schemaType of
      Nothing -> top
      Just OpenApiNull -> bottom
        { forNull = top }
      Just OpenApiBoolean -> bottom
        { forBoolean = top }
      Just OpenApiNumber -> bottom
        { forBoolean = top }
      Just OpenApiInteger -> bottom
        { forNumber = singletonFormula (t `Snoc` IntegerType) $ MultipleOf 1 }
      Just OpenApiString -> bottom
        { forString = top }
      Just OpenApiArray -> bottom
        { forArray = top }
      Just OpenApiObject -> bottom
        { forObject = top }

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
        { forNumber = singletonFormula (t `Snoc` MaximumFields) $ Maximum $
          case _schemaExclusiveMaximum of
            Just True -> Exclusive n
            _ -> Inclusive n }

    minimumClause = case _schemaMinimum of
      Nothing -> top
      Just n -> top
        { forNumber = singletonFormula (t `Snoc` MinimumFields) $ Minimum $ Down $
          case _schemaExclusiveMinimum of
            Just True -> Exclusive $ Down n
            _ -> Inclusive $ Down n }

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
    Just (OpenApiItemsObject rs) -> do
      let trs = Traced (t `Snoc` ItemsStep) rs
      f <- processRefSchema trs
      pure top { forArray = singletonFormula (t `Snoc` ItemsField) $ Items f trs }
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

  (addProps, addPropSchema) <- case _schemaAdditionalProperties of
    Just (AdditionalPropertiesSchema rs) -> do
      let trs = Traced (t `Snoc` AdditionalPropertiesStep) rs
      (,Just trs) <$> processRefSchema trs
    Just (AdditionalPropertiesAllowed False) -> pure (bottom, Nothing)
    _ -> pure (top, Just $ Traced (t `Snoc` AdditionalPropertiesStep) $ Inline mempty)
  propList <- forM (S.toList . S.fromList $ IOHM.keys _schemaProperties <> _schemaRequired) $ \k -> do
    (f, sch) <- case IOHM.lookup k _schemaProperties of
      Just rs -> do
        let trs = Traced (t `Snoc` PropertiesStep k) rs
        (,trs) <$> processRefSchema trs
      Nothing -> pure (addProps, fromMaybe (Traced (t `Snoc` AdditionalPropertiesStep) $ Inline mempty) addPropSchema)
      -- The mempty here is incorrect, but if addPropSchema was Nothing, then
      -- addProps is bottom, and k is in _schemaRequired. We handle this situation
      -- below and short-circuit the entire Properties condition to bottom
    pure (k, Property (k `elem` _schemaRequired) f sch)
  let
    allBottom f = getAll $ foldType $ \ty -> case ty f of
      BottomFormula -> All True
      _ -> All False
    allTop f = getAll $ foldType $ \ty -> case ty f of
      TopFormula -> All True
      _ -> All False
    -- remove optional fields whose schemata match that of additional props
    propMap = M.filter (\p -> propRequired p || propFormula p /= addProps) $ M.fromList propList
    propertiesClause
      | any (\p -> propRequired p && allBottom (propFormula p)) propMap
      = bottom -- if any required field has unsatisfiable schema
      | M.null propMap, allTop addProps
      = top -- if all fields are optional and have trivial schemata
      | otherwise
      = top
        { forObject = singletonFormula (t `Snoc` PropertiesFields) $ Properties propMap addProps addPropSchema }

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
    [ anyClause, oneClause, typeClause, enumClause, maximumClause, minimumClause
    , multipleOfClause, formatClause, maxLengthClause, minLengthClause
    , patternClause, itemsClause, maxItemsClause, minItemsClause
    , uniqueItemsClause, propertiesClause, maxPropertiesClause, minPropertiesClause])
{- TODO: ReadOnly/WriteOnly -}

checkOneOfDisjoint :: [Referenced Schema] -> ProcessM Bool
checkOneOfDisjoint = const $ pure True -- TODO

schemaToFormula
  :: Definitions Schema
  -> Traced OpenApi Schema
  -> (ForeachType (JsonFormula OpenApi), T.TracePrefixTree SubtreeCheckIssue OpenApi)
schemaToFormula defs rs = runWriter . (`runReaderT` defs) $ processSchema rs

checkFormulas
  :: HasAll (CheckEnv Schema) xs
  => HList xs
  -> Trace OpenApi Schema
  -> ProdCons (ForeachType (JsonFormula OpenApi), T.TracePrefixTree SubtreeCheckIssue OpenApi)
  -> CompatFormula Schema ()
checkFormulas env tr (ProdCons (fp, ep) (fc, ec)) =
  case T.toList ep ++ T.toList ec of
    issues@(_:_) -> F.for_ issues $ \(AnItem t (SubtreeCheckIssue e)) -> issueAtTrace t e
    [] -> do
      -- We have the following isomorphisms:
      --   (A ⊂ X ∪ Y) = (A ⊂ X) \/ (A ⊂ Y)
      --   (A ⊂ X ∩ Y) = (A ⊂ X) /\ (A ⊂ Y)
      --   (A ⊂ ⊤) = 1
      --   (X ∪ Y ⊂ B) = (X ⊂ B) /\ (Y ⊂ B)
      --   (∅ ⊂ B) = 1
      -- The remaining cases are, notably, not isomorphisms:
      --   1) (A ⊂ ∅) <= 0
      --   2) (X ∩ Y ⊂ B) <= (X ⊂ B) \/ (Y ⊂ B)
      --   3) (⊤ ⊂ B) <= 0
      -- Therefore we have the isomorphisms:
      --   (⋃_i ⋂_j A[i,j]) ⊂ (⋃_k ⋂_l B[k,l])
      --   = \/_k /\_l /\_i (⋂_j A[i,j]) ⊂ B[k,l]
      --   = /\_i \/_k /\_l (⋂_j A[i,j]) ⊂ B[k,l]
      -- with the caveat that the the set over which k ranges is nonempty.
      -- This is because 1) is not an isomorphism.
      -- Our disjunction loses information, so it makes sense to nest it as
      -- deeply as possible, hence we choose the latter representation.
      --
      -- We delegate the verification of (⋂_j A[j]) ⊂ B to a separate heuristic
      -- function, with the understanding that \/_j A[j] ⊂ B is a sufficient,
      -- but not necessary condition (because of 2) and 3)).
      --
      -- If k ranges over an empty set, we have the isomorphism:
      --   (⋃_i ⋂_j A[i,j]) ⊂ ∅ = /\_i (⋂_j A[i,j]) ⊂ ∅
      -- where we again delegate (⋂_j A[j]) ⊂ ∅ to a heuristic, though here the
      -- shortcut of \/_j A[j] ⊂ ∅ hardly helps.
      forType_ $ \ty ->
        case (ty fp, ty fc) of
          (DNF pss, BottomFormula) -> F.for_ pss $ \(Conjunct ps) -> checkContradiction tr ps
          (DNF pss, SingleConjunct cs) -> F.for_ pss $ \(Conjunct ps) -> do
            F.for_ cs $ checkImplication env ps -- avoid disjuntion if there's only one conjunct
          (DNF pss, DNF css) -> F.for_ pss $ \(Conjunct ps) -> do
            anyOfM tr (SubtreeCheckIssue $ NoMatchingCondition $ SomeCondition . getTraced <$> ps)
              [F.for_ cs $ checkImplication env ps | Conjunct cs <- S.toList css]

checkContradiction
  :: Trace OpenApi Schema
  -> [Traced OpenApi (Condition t)]
  -> CompatFormula s ()
checkContradiction tr _ = issueAtTrace tr NoContradiction -- TODO

checkImplication
  :: (HasAll (CheckEnv Schema) xs, Typeable t)
  => HList xs
  -> [Traced OpenApi (Condition t)]
  -> Traced OpenApi (Condition t)
  -> CompatFormula s ()
checkImplication env prods (Traced t cons) = case findExactly prods of
  Just e
    | all (satisfiesTyped e) (getTraced <$> prods) ->
      if satisfiesTyped e cons then pure ()
        else issueAtTrace t (EnumDoesntSatisfy e)
    | otherwise -> pure () -- vacuously true
  Nothing -> case cons of
    -- the above code didn't catch it, so there's no Exactly condition on the lhs
    Exactly e -> issueAtTrace t (NoMatchingEnum e)
    Maximum m -> case findRelevant min (\case Maximum m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' <= m then pure ()
        else issueAtTrace t (MatchingMaximumWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMaximum m)
    Minimum m -> case findRelevant max (\case Minimum m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' >= m then pure ()
        else issueAtTrace t (MatchingMinimumWeak (coerce m) (coerce m'))
      Nothing -> issueAtTrace t (NoMatchingMinimum (coerce m))
    MultipleOf m -> case findRelevant lcmScientific (\case MultipleOf m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if lcmScientific m m' == m' then pure ()
        else issueAtTrace t (MatchingMultipleOfWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMultipleOf m)
    NumberFormat f -> if any (\case NumberFormat f' -> f == f'; _ -> False) $ getTraced <$> prods
      then pure () else issueAtTrace t (NoMatchingFormat f)
    MaxLength m -> case findRelevant min (\case MaxLength m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' <= m then pure ()
        else issueAtTrace t (MatchingMaxLengthWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMaxLength m)
    MinLength m -> case findRelevant max (\case MinLength m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' >= m then pure ()
        else issueAtTrace t (MatchingMinLengthWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMinLength m)
    Pattern p -> if any (\case Pattern p' -> p == p'; _ -> False) $ getTraced <$> prods
      then pure () else issueAtTrace t (NoMatchingPattern p)
    StringFormat f -> if any (\case StringFormat f' -> f == f'; _ -> False) $ getTraced <$> prods
      then pure () else issueAtTrace t (NoMatchingFormat f)
    Items _ (Traced t' cons') -> case findRelevant (<>) (\case Items _ rs -> Just (rs NE.:| []); _ -> Nothing) prods of
      Just (rs NE.:| []) -> localTrace' (ProdCons (getTrace rs) t') $ checkCompatibility env $ ProdCons (getTraced rs) cons'
      Just rs -> do
        let sch = Inline mempty { _schemaAllOf = Just . NE.toList $ getTraced <$> rs }
        localTrace' (pure t' {- TODO: what? -}) $ checkCompatibility env $ ProdCons sch cons'
      Nothing -> issueAtTrace t NoMatchingItems
    MaxItems m -> case findRelevant min (\case MaxItems m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' <= m then pure ()
        else issueAtTrace t (MatchingMaxItemsWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMaxItems m)
    MinItems m -> case findRelevant max (\case MinItems m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' >= m then pure ()
        else issueAtTrace t (MatchingMinItemsWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMinItems m)
    UniqueItems -> if any ((== UniqueItems) . getTraced) prods then pure ()
      else issueAtTrace t NoMatchingUniqueItems
    Properties props _ madd -> case findRelevant (<>) (\case Properties props' _ madd' -> Just $ (props', madd') NE.:| []; _ -> Nothing) prods of
      Just ((props', madd') NE.:| []) -> do
        F.for_ (S.fromList $ M.keys props <> M.keys props') $ \k -> do
          let
            go sch' sch = let schs = ProdCons sch' sch
              in localTrace' (getTrace <$> schs) $ checkCompatibility env $ getTraced <$> schs
          case (M.lookup k props', madd', M.lookup k props, madd) of
            (Nothing, Nothing, _, _) -> pure () -- vacuously
            (_, _, Nothing, Nothing) -> issueAtTrace t (UnexpectedProperty k)
            (Just p', _, Just p, _) -> go (propRefSchema p') (propRefSchema p)
            (Nothing, Just add', Just p, _) -> go add' (propRefSchema p)
            (Just p', _, Nothing, Just add) -> go (propRefSchema p') add
            (Nothing, Just _, Nothing, Just _) -> pure ()
          case (maybe False propRequired $ M.lookup k props', maybe False propRequired $ M.lookup k props) of
            (False, True) -> issueAtTrace t (PropertyNowRequired k)
            _ -> pure ()
          pure ()
        case (madd', madd) of
          (Nothing, _) -> pure () -- vacuously
          (_, Nothing) -> issueAtTrace t NoAdditionalProperties
          (Just add', Just add) -> let schs = ProdCons add' add
            in localTrace' (getTrace <$> schs) $ checkCompatibility env $ getTraced <$> schs
        pure ()
      Nothing -> issueAtTrace t NoMatchingProperties
    MaxProperties m -> case findRelevant min (\case MaxProperties m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' <= m then pure ()
        else issueAtTrace t (MatchingMaxPropertiesWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMaxProperties m)
    MinProperties m -> case findRelevant max (\case MinProperties m' -> Just m'; _ -> Nothing) prods of
      Just m' -> if m' >= m then pure ()
        else issueAtTrace t (MatchingMinPropertiesWeak m m')
      Nothing -> issueAtTrace t (NoMatchingMinProperties m)
  where
    findExactly (Traced _ (Exactly x):_) = Just x
    findExactly (_:xs) = findExactly xs
    findExactly [] = Nothing
    findRelevant combine extract
      = fmap (foldr1 combine) . NE.nonEmpty . mapMaybe (extract . getTraced)
    lcmScientific (toRational -> a) (toRational -> b)
      = fromRational $ lcm (numerator a) (numerator b) % gcd (denominator a) (denominator b)

instance Typeable t => Subtree (Condition t) where
  data CheckIssue (Condition t)
    = EnumDoesntSatisfy (TypedValue t)
    | NoMatchingEnum (TypedValue t)
    | NoMatchingMaximum (Bound Scientific)
    | MatchingMaximumWeak (Bound Scientific) (Bound Scientific)
    | NoMatchingMinimum (Bound Scientific)
    | MatchingMinimumWeak (Bound Scientific) (Bound Scientific)
    | NoMatchingMultipleOf Scientific
    | MatchingMultipleOfWeak Scientific Scientific
    | NoMatchingFormat Format
    | NoMatchingMaxLength Integer
    | MatchingMaxLengthWeak Integer Integer
    | NoMatchingMinLength Integer
    | MatchingMinLengthWeak Integer Integer
    | NoMatchingPattern Pattern
    | NoMatchingItems
    | NoMatchingMaxItems Integer
    | MatchingMaxItemsWeak Integer Integer
    | NoMatchingMinItems Integer
    | MatchingMinItemsWeak Integer Integer
    | NoMatchingUniqueItems
    | NoMatchingProperties
    | UnexpectedProperty Text
    | PropertyNowRequired Text
    | NoAdditionalProperties
    | NoMatchingMaxProperties Integer
    | MatchingMaxPropertiesWeak Integer Integer
    | NoMatchingMinProperties Integer
    | MatchingMinPropertiesWeak Integer Integer
    deriving stock (Eq, Ord, Show)
  type CheckEnv (Condition t) = CheckEnv Schema
  normalizeTrace = undefined
  checkCompatibility env conds = withTrace $ \traces -> do
    case Traced <$> traces <*> conds of
      ProdCons prod cons -> checkImplication env [prod] cons

instance Subtree Schema where
  data CheckIssue Schema
    = NotSupported Text
    | InvalidSchema Text
    | NoMatchingCondition [SomeCondition]
    | NoContradiction
    deriving stock (Eq, Ord, Show)
  type CheckEnv Schema = '[ProdCons (Definitions Schema)]
  checkCompatibility env schs = withTrace $ \traces -> do
    let defs = getH env
    checkFormulas env (producer traces) $ schemaToFormula <$> defs <*> (Traced <$> traces <*> schs)

instance Subtree (Referenced Schema) where
  data CheckIssue (Referenced Schema)
    deriving stock (Eq, Ord, Show)
  type CheckEnv (Referenced Schema) = CheckEnv Schema
  checkCompatibility env refs = withTrace $ \traces -> do
    let
      defs = getH env
      schs = dereference <$> defs <*> refs
      schs' = retrace <$> traces <*> schs
    localTrace (getTrace <$> schs) $ do
      checkFormulas env (producer $ getTrace <$> schs') $ schemaToFormula <$> defs <*> schs'
