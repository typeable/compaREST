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
  , Behave (..)
  , describeJSONType
  )
where

import Algebra.Lattice
import Algebra.Lattice.Lifted
import Control.Applicative
import Control.Arrow
import Control.Comonad.Env hiding (env)
import Control.Monad.Reader hiding (ask)
import qualified Control.Monad.Reader as R
import Control.Monad.State
import qualified Control.Monad.Trans.Reader as R (liftCatch)
import qualified Control.Monad.Trans.Writer as W (liftCatch)
import Control.Monad.Writer
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce
import Data.Foldable
import qualified Data.Foldable as F
import Data.Functor
import Data.Functor.Identity
import Data.HList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Int
import Data.Kind
import Data.List (sortBy)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi hiding (get)
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T hiding (singleton)
import qualified Data.Text.Encoding as T
import Data.Typeable
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Memo
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Paths
import qualified OpenAPI.Checker.PathsPrefixTree as P
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import Text.Pandoc.Builder hiding (Format, Null)
import Text.Regex.Pcre2

-- | Type of a JSON value
data JsonType
  = Null
  | Boolean
  | Number
  | String
  | Array
  | Object
  deriving stock (Eq, Ord, Show)

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
  deriving stock (Eq, Show, Functor)

-- | The order is lexicographical on @a * Bool@.
instance Ord a => Ord (Bound a) where
  Exclusive a `compare` Exclusive b = compare a b
  Exclusive a `compare` Inclusive b = if a <= b then LT else GT
  Inclusive a `compare` Exclusive b = if a < b then LT else GT
  Inclusive a `compare` Inclusive b = compare a b

data Property = Property
  { propRequired :: Bool
  , propFormula :: ForeachType JsonFormula
  , propRefSchema :: Traced (Referenced Schema)
  }
  deriving stock (Eq, Ord, Show)

-- | A primitive structural condition for the "top level" of a JSON value (of a specific type)
data Condition :: JsonType -> Type where
  Exactly :: TypedValue t -> Condition t
  Maximum :: !(Bound Scientific) -> Condition 'Number
  Minimum
    :: !(Down (Bound (Down Scientific)))
    -> Condition 'Number -- ^ this has the right Ord
  MultipleOf :: !Scientific -> Condition 'Number
  NumberFormat :: !Format -> Condition 'Number
  MaxLength :: !Integer -> Condition 'String
  MinLength :: !Integer -> Condition 'String
  Pattern :: !Pattern -> Condition 'String
  StringFormat :: !Format -> Condition 'String
  Items
    :: !(ForeachType JsonFormula)
    -> !(Traced (Referenced Schema))
    -> Condition 'Array
  MaxItems :: !Integer -> Condition 'Array
  MinItems :: !Integer -> Condition 'Array
  UniqueItems :: Condition 'Array
  Properties
    :: !(M.Map Text Property)
    -> !(ForeachType JsonFormula) -- ^ formula for additional properties
    -> !(Maybe (Traced (Referenced Schema))) -- ^ schema for additional properties, Nothing means bottom
    -> Condition 'Object
  MaxProperties :: !Integer -> Condition 'Object
  MinProperties :: !Integer -> Condition 'Object

showCondition :: Condition a -> Blocks
showCondition = \case
  (Exactly v) -> para "The value should be:" <> showJSONValue (untypeValue v)
  (Maximum b) -> para $ "The value should be less than " <> showBound b <> "."
  (Minimum (Down b)) -> para $ "The value should be more than " <> showBound (getDown <$> b) <> "."
  (MultipleOf n) -> para $ "The value should be a multiple of " <> show' n <> "."
  (NumberFormat p) -> para $ "The number should have the following format:" <> code p <> "."
  (Pattern p) -> para "The value should satisfy the following pattern (regular expression):" <> codeBlock p
  (StringFormat p) -> para $ "The string should have the following format:" <> code p <> "."
  (MaxLength p) -> para $ "The length of the string should be less than or equal to " <> show' p <> "."
  (MinLength p) -> para $ "The length of the string should be more than or equal to " <> show' p <> "."
  (Items i _) -> para "The items of the array should satisfy:" <> showForEachJsonFormula i
  (MaxItems n) -> para $ "The length of the array should be less than or equal to " <> show' n <> "."
  (MinItems n) -> para $ "The length of the array should be more than or equal to " <> show' n <> "."
  UniqueItems -> para "The elements in the array should be unique."
  (Properties props additional _) ->
    bulletList $
      (M.toList props
         <&> (\(k, p) ->
                para (code k)
                  <> para (strong $ if propRequired p then "Required" else "Optional")
                  <> showForEachJsonFormula (propFormula p)))
        <> [ para (emph "Additional properties")
               <> showForEachJsonFormula additional
           ]
  (MaxProperties n) -> para $ "The maximum number of fields should be " <> show' n <> "."
  (MinProperties n) -> para $ "The minimum number of fields should be " <> show' n <> "."
  where
    showForEachJsonFormula :: ForeachType JsonFormula -> Blocks
    showForEachJsonFormula i =
      bulletList $
        foldType
          (\t f -> case getJsonFormula $ f i of
             BottomFormula -> mempty
             (DNF conds') ->
               let conds = S.toList <$> S.toList conds'
                in [ para (describeJSONType t)
                       <> bulletList
                         (conds <&> \case
                            [] -> para "Empty"
                            cond -> bulletList (showCondition <$> cond))
                   ])

satisfiesTyped :: TypedValue t -> Condition t -> Bool
satisfiesTyped e (Exactly e') = e == e'
satisfiesTyped (TNumber n) (Maximum (Exclusive m)) = n < m
satisfiesTyped (TNumber n) (Maximum (Inclusive m)) = n <= m
satisfiesTyped (TNumber n) (Minimum (Down (Exclusive (Down m)))) = n > m
satisfiesTyped (TNumber n) (Minimum (Down (Inclusive (Down m)))) = n >= m
satisfiesTyped (TNumber n) (MultipleOf m) = denominator (toRational n / toRational m) == 1 -- TODO: could be better #36
satisfiesTyped (TNumber n) (NumberFormat f) = checkNumberFormat f n
satisfiesTyped (TString s) (MaxLength m) = fromIntegral (T.length s) <= m
satisfiesTyped (TString s) (MinLength m) = fromIntegral (T.length s) >= m
satisfiesTyped (TString s) (Pattern p) = isJust $ match p s -- TODO: regex stuff #32
satisfiesTyped (TString s) (StringFormat f) = checkStringFormat f s
satisfiesTyped (TArray a) (Items f _) = all (`satisfies` f) a
satisfiesTyped (TArray a) (MaxItems m) = fromIntegral (F.length a) <= m
satisfiesTyped (TArray a) (MinItems m) = fromIntegral (F.length a) >= m
satisfiesTyped (TArray a) UniqueItems = S.size (S.fromList $ F.toList a) == F.length a -- TODO: could be better #36
satisfiesTyped (TObject o) (Properties props additional _) =
  all (`HM.member` o) (M.keys (M.filter propRequired props))
    && all (\(k, v) -> satisfies v $ maybe additional propFormula $ M.lookup k props) (HM.toList o)
satisfiesTyped (TObject o) (MaxProperties m) = fromIntegral (HM.size o) <= m
satisfiesTyped (TObject o) (MinProperties m) = fromIntegral (HM.size o) >= m

checkNumberFormat :: Format -> Scientific -> Bool
checkNumberFormat "int32" (toRational -> n) =
  denominator n == 1
    && n >= toRational (minBound :: Int32)
    && n <= toRational (maxBound :: Int32)
checkNumberFormat "int64" (toRational -> n) =
  denominator n == 1
    && n >= toRational (minBound :: Int64)
    && n <= toRational (maxBound :: Int64)
checkNumberFormat "float" _n = True
checkNumberFormat "double" _n = True
checkNumberFormat f _n = error $ "Invalid number format: " <> T.unpack f

checkStringFormat :: Format -> Text -> Bool
checkStringFormat "byte" _s = True -- TODO: regex stuff #32
checkStringFormat "binary" _s = True
checkStringFormat "date" _s = True
checkStringFormat "date-time" _s = True
checkStringFormat "password" _s = True
checkStringFormat "uuid" _s = True
checkStringFormat f _s = error $ "Invalid string format: " <> T.unpack f

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

-- | A boolean formula (without "not") represented as a Disjunctive Normal Form:
-- the formula is a disjunction of a set of conjuncts, each of which is a
-- conjunction of a set of some elementary formulas.
newtype DNF a
  = DNF (S.Set (S.Set a))
  deriving stock (Eq, Ord, Show)

disjAdd :: Ord a => DNF a -> S.Set a -> DNF a
disjAdd (DNF yss) xs
  | any (`S.isSubsetOf` xs) yss = DNF yss
  | otherwise = DNF $ S.insert xs $ S.filter (not . S.isSubsetOf xs) yss

instance Ord a => Lattice (DNF a) where
  xss \/ DNF yss = S.foldl' disjAdd xss yss
  DNF xss /\ DNF yss =
    F.foldl' disjAdd bottom $
      liftA2 S.union (S.toList xss) (S.toList yss)

pattern BottomFormula :: DNF a
pattern BottomFormula <-
  DNF (S.null -> True)
  where
    BottomFormula = DNF S.empty

isSingleton :: S.Set a -> Maybe a
isSingleton s
  | S.size s == 1 = S.lookupMin s
  | otherwise = Nothing

pattern Conjunct :: Ord a => [a] -> S.Set a
pattern Conjunct xs <-
  (S.toList -> xs)
  where
    Conjunct = S.fromList

{-# COMPLETE Conjunct #-}

pattern SingleConjunct :: Ord a => [a] -> DNF a
pattern SingleConjunct xs <-
  DNF (isSingleton -> Just (Conjunct xs))
  where
    SingleConjunct xs = DNF $ S.singleton $ Conjunct xs

pattern TopFormula :: DNF a
pattern TopFormula <-
  DNF (isSingleton -> Just (S.null -> True))
  where
    TopFormula = DNF $ S.singleton S.empty

instance Ord a => BoundedJoinSemiLattice (DNF a) where
  bottom = BottomFormula

instance Ord a => BoundedMeetSemiLattice (DNF a) where
  top = TopFormula

foldLattice
  :: BoundedLattice l
  => (a -> l)
  -> DNF a
  -> l
foldLattice f (DNF xss) =
  S.foldl'
    (\z w ->
       z \/ S.foldl' (\x y -> x /\ f y) top w)
    bottom
    xss

newtype JsonFormula t = JsonFormula {getJsonFormula :: DNF (Condition t)}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice)

satisfiesFormula :: TypedValue t -> JsonFormula t -> Bool
satisfiesFormula val = foldLattice (satisfiesTyped val) . getJsonFormula

data ForeachType (f :: JsonType -> Type) = ForeachType
  { forNull :: f 'Null
  , forBoolean :: f 'Boolean
  , forNumber :: f 'Number
  , forString :: f 'String
  , forArray :: f 'Array
  , forObject :: f 'Object
  }

satisfies :: A.Value -> ForeachType JsonFormula -> Bool
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
  => (forall x. Typeable x => JsonType -> (ForeachType f -> f x) -> m)
  -> m
foldType k =
  k Null forNull
    <> k Boolean forBoolean
    <> k Number forNumber
    <> k String forString
    <> k Array forArray
    <> k Object forObject

forType_
  :: Applicative m
  => (forall x. Typeable x => JsonType -> (ForeachType f -> f x) -> m ())
  -> m ()
forType_ k = do
  k Null forNull
  k Boolean forBoolean
  k Number forNumber
  k String forString
  k Array forArray
  k Object forObject
  pure ()

instance (forall x. Lattice (f x)) => Lattice (ForeachType f) where
  f1 \/ f2 =
    ForeachType
      { forNull = forNull f1 \/ forNull f2
      , forBoolean = forBoolean f1 \/ forBoolean f2
      , forNumber = forNumber f1 \/ forNumber f2
      , forString = forString f1 \/ forString f2
      , forArray = forArray f1 \/ forArray f2
      , forObject = forObject f1 \/ forObject f2
      }
  f1 /\ f2 =
    ForeachType
      { forNull = forNull f1 /\ forNull f2
      , forBoolean = forBoolean f1 /\ forBoolean f2
      , forNumber = forNumber f1 /\ forNumber f2
      , forString = forString f1 /\ forString f2
      , forArray = forArray f1 /\ forArray f2
      , forObject = forObject f1 /\ forObject f2
      }

instance
  (forall x. BoundedJoinSemiLattice (f x))
  => BoundedJoinSemiLattice (ForeachType f)
  where
  bottom =
    ForeachType
      { forNull = bottom
      , forBoolean = bottom
      , forNumber = bottom
      , forString = bottom
      , forArray = bottom
      , forObject = bottom
      }

instance
  (forall x. BoundedMeetSemiLattice (f x))
  => BoundedMeetSemiLattice (ForeachType f)
  where
  top =
    ForeachType
      { forNull = top
      , forBoolean = top
      , forNumber = top
      , forString = top
      , forArray = top
      , forObject = top
      }

instance Steppable Schema (Referenced Schema) where
  data Step Schema (Referenced Schema)
    = AllOfStep Int
    | OneOfStep Int
    | AnyOfStep Int
    | ItemsObjectStep
    | ItemsArrayStep Int
    | AdditionalPropertiesStep
    | NotStep
    deriving stock (Eq, Ord, Show)

instance Steppable (Referenced Schema) (Referenced Schema) where
  data Step (Referenced Schema) (Referenced Schema)
    = -- | Invariant (for better memoization only): the "tail" of the trace is
      -- the "least" of the traces of the conjuncted schemata
      ConjunctedWith (NE.NonEmpty (Trace (Referenced Schema)))
    | Partitioned PartitionLocation PartitionChoice
    deriving stock (Eq, Ord, Show)

instance Steppable Schema (Definitions (Referenced Schema)) where
  data Step Schema (Definitions (Referenced Schema)) = PropertiesStep
    deriving stock (Eq, Ord, Show)

instance Steppable Schema Discriminator where
  data Step Schema Discriminator = DiscriminatorStep
    deriving stock (Eq, Ord, Show)

instance Steppable Discriminator (Definitions (Referenced Schema)) where
  data Step Discriminator (Definitions (Referenced Schema)) = DiscriminatorMapping
    deriving stock (Eq, Ord, Show)

parseDiscriminatorValue :: Text -> Referenced Schema
parseDiscriminatorValue v = case A.fromJSON @(Referenced Schema) $ A.object ["$ref" A..= v] of
  A.Success x -> x
  A.Error _ -> Ref $ Reference v

-- | A fake writer monad that doesn't actually record anything and allows lazy recursion.
newtype Silent q f r a = Silent {runSilent :: a}
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance MonadWriter (P.PathsPrefixTree q f r) (Silent q f r) where
  tell _ = Silent ()
  listen (Silent x) = Silent (x, P.empty)
  pass (Silent (x, _)) = Silent x

instance MonadState (MemoState ()) (Silent q f r) where
  get = Silent $ runIdentity $ runMemo () get
  put _ = pure ()

type MonadProcess m =
  ( MonadReader (Traced (Definitions Schema)) m
  , MonadWriter (P.PathsPrefixTree Behave AnIssue 'SchemaLevel) m
  , MonadState (MemoState ()) m
  )

type SilentM = ReaderT (Traced (Definitions Schema)) (Silent Behave AnIssue 'SchemaLevel)

warn :: MonadProcess m => Issue 'SchemaLevel -> m ()
warn issue = tell $ P.singleton $ AnItem Root $ anIssue issue

-- | Ignore warnings but allow a recursive loop that lazily computes a recursive 'Condition'.
silently :: MonadProcess m => SilentM a -> m a
silently m = do
  defs <- R.ask
  pure . runSilent $ runReaderT m defs

warnKnot :: MonadProcess m => KnotTier (ForeachType JsonFormula) () m
warnKnot =
  KnotTier
    { onKnotFound = warn UnguardedRecursion
    , onKnotUsed = \_ -> pure bottom
    , tieKnot = \_ -> pure
    }

processRefSchema
  :: MonadProcess m
  => Traced (Referenced Schema)
  -> m (ForeachType JsonFormula)
processRefSchema x = do
  defs <- R.ask
  memoWithKnot warnKnot (processSchema $ dereference defs x) (ask x)

tracedAllOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedAllOf sch =
  _schemaAllOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (AllOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedAnyOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedAnyOf sch =
  _schemaAnyOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (AnyOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedOneOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedOneOf sch =
  _schemaOneOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (OneOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedItems :: Traced Schema -> Maybe (Either (Traced (Referenced Schema)) [Traced (Referenced Schema)])
tracedItems sch =
  _schemaItems (extract sch) <&> \case
    OpenApiItemsObject x -> Left $ traced (ask sch >>> step ItemsObjectStep) x
    OpenApiItemsArray xs ->
      Right
        [traced (ask sch >>> step (ItemsArrayStep i)) x | (i, x) <- zip [0 ..] xs]

tracedAdditionalProperties :: Traced Schema -> Maybe (Either Bool (Traced (Referenced Schema)))
tracedAdditionalProperties sch =
  _schemaAdditionalProperties (extract sch) <&> \case
    AdditionalPropertiesAllowed b -> Left b
    AdditionalPropertiesSchema x -> Right $ traced (ask sch >>> step AdditionalPropertiesStep) x

tracedDiscriminator :: Traced Schema -> Maybe (Traced Discriminator)
tracedDiscriminator = sequence . stepTraced DiscriminatorStep . fmap _schemaDiscriminator

tracedProperties :: Traced Schema -> IOHM.InsOrdHashMap Text (Traced (Referenced Schema))
tracedProperties sch =
  IOHM.mapWithKey
    (\k -> traced (ask sch >>> step PropertiesStep >>> step (InsOrdHashMapKeyStep k)))
    (_schemaProperties $ extract sch)

-- | Turn a schema into a tuple of 'JsonFormula's that describes the condition
-- for every possible type of a JSON value. The conditions are independent, and
-- are thus checked independently.
processSchema
  :: MonadProcess m
  => Traced Schema
  -> m (ForeachType JsonFormula)
processSchema sch@(extract -> Schema {..}) = do
  let singletonFormula :: Condition t -> JsonFormula t
      singletonFormula f = JsonFormula $ SingleConjunct [f]

  allClauses <- case tracedAllOf sch of
    Nothing -> pure []
    Just [] -> [] <$ warn (InvalidSchema "no items in allOf")
    Just xs -> mapM processRefSchema xs

  anyClause <- case tracedAnyOf sch of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in anyOf")
    Just xs -> joins <$> mapM processRefSchema xs

  oneClause <- case tracedOneOf sch of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in oneOf")
    Just xs -> do
      checkOneOfDisjoint xs >>= \case
        True -> pure ()
        False -> warn (NotSupported "Could not determine that oneOf branches are disjoint")
      joins <$> mapM processRefSchema xs

  case _schemaNot of
    Nothing -> pure ()
    Just _ -> warn (NotSupported "not clause is unsupported")

  let typeClause = case _schemaType of
        Nothing -> top
        Just OpenApiNull ->
          bottom
            { forNull = top
            }
        Just OpenApiBoolean ->
          bottom
            { forBoolean = top
            }
        Just OpenApiNumber ->
          bottom
            { forNumber = top
            }
        Just OpenApiInteger ->
          bottom
            { forNumber = singletonFormula $ MultipleOf 1
            }
        Just OpenApiString ->
          bottom
            { forString = top
            }
        Just OpenApiArray ->
          bottom
            { forArray = top
            }
        Just OpenApiObject ->
          bottom
            { forObject = top
            }

  let valueEnum A.Null =
        bottom
          { forNull = singletonFormula $ Exactly TNull
          }
      valueEnum (A.Bool b) =
        bottom
          { forBoolean = singletonFormula $ Exactly $ TBool b
          }
      valueEnum (A.Number n) =
        bottom
          { forNumber = singletonFormula $ Exactly $ TNumber n
          }
      valueEnum (A.String s) =
        bottom
          { forString = singletonFormula $ Exactly $ TString s
          }
      valueEnum (A.Array a) =
        bottom
          { forArray = singletonFormula $ Exactly $ TArray a
          }
      valueEnum (A.Object o) =
        bottom
          { forObject = singletonFormula $ Exactly $ TObject o
          }
  enumClause <- case _schemaEnum of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in enum")
    Just xs -> pure $ joins (valueEnum <$> xs)

  let maximumClause = case _schemaMaximum of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $
                Maximum $
                  case _schemaExclusiveMaximum of
                    Just True -> Exclusive n
                    _ -> Inclusive n
            }

      minimumClause = case _schemaMinimum of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $
                Minimum $
                  Down $
                    case _schemaExclusiveMinimum of
                      Just True -> Exclusive $ Down n
                      _ -> Inclusive $ Down n
            }

      multipleOfClause = case _schemaMultipleOf of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $ MultipleOf n
            }

  formatClause <- case _schemaFormat of
    Nothing -> pure top
    Just f
      | f `elem` ["int32", "int64", "float", "double"] ->
        pure
          top
            { forNumber = singletonFormula $ NumberFormat f
            }
    Just f
      | f `elem` ["byte", "binary", "date", "date-time", "password", "uuid"] ->
        pure
          top
            { forString = singletonFormula $ StringFormat f
            }
    Just f -> top <$ warn (NotSupported $ "Unknown format: " <> f)

  let maxLengthClause = case _schemaMaxLength of
        Nothing -> top
        Just n ->
          top
            { forString = singletonFormula $ MaxLength n
            }

      minLengthClause = case _schemaMinLength of
        Nothing -> top
        Just n ->
          top
            { forString = singletonFormula $ MinLength n
            }

      patternClause = case _schemaPattern of
        Nothing -> top
        Just p ->
          top
            { forString = singletonFormula $ Pattern p
            }

  itemsClause <- case tracedItems sch of
    Nothing -> pure top
    Just (Left rs) -> do
      f <- silently $ processRefSchema rs
      pure top {forArray = singletonFormula $ Items f rs}
    Just (Right _) -> top <$ warn (NotSupported "array in items is not supported")

  let maxItemsClause = case _schemaMaxItems of
        Nothing -> top
        Just n ->
          top
            { forArray = singletonFormula $ MaxItems n
            }

      minItemsClause = case _schemaMinItems of
        Nothing -> top
        Just n ->
          top
            { forArray = singletonFormula $ MinItems n
            }

      uniqueItemsClause = case _schemaUniqueItems of
        Just True ->
          top
            { forArray = singletonFormula UniqueItems
            }
        _ -> top

  (addProps, addPropSchema) <- case tracedAdditionalProperties sch of
    Just (Right rs) -> (,Just rs) <$> silently (processRefSchema rs)
    Just (Left False) -> pure (bottom, Nothing)
    _ -> pure (top, Just $ traced (ask sch `Snoc` AdditionalPropertiesStep) $ Inline mempty)
  propList <- forM (S.toList . S.fromList $ IOHM.keys _schemaProperties <> _schemaRequired) $ \k -> do
    (f, psch) <- case IOHM.lookup k $ tracedProperties sch of
      Just rs -> (,rs) <$> silently (processRefSchema rs)
      Nothing ->
        let fakeSchema = traced (ask sch `Snoc` AdditionalPropertiesStep) $ Inline mempty
         in -- The mempty here is incorrect, but if addPropSchema was Nothing, then
            -- addProps is bottom, and k is in _schemaRequired. We handle this situation
            -- below and short-circuit the entire Properties condition to bottom
            pure (addProps, fromMaybe fakeSchema addPropSchema)
    pure (k, Property (k `elem` _schemaRequired) f psch)
  let allBottom f = getAll $
        foldType $ \_ ty -> case getJsonFormula $ ty f of
          BottomFormula -> All True
          _ -> All False
      allTop f = getAll $
        foldType $ \_ ty -> case getJsonFormula $ ty f of
          TopFormula -> All True
          _ -> All False
      -- remove optional fields whose schemata match that of additional props
      propMap = M.filter (\p -> propRequired p || propFormula p /= addProps) $ M.fromList propList
      propertiesClause
        | any (\p -> propRequired p && allBottom (propFormula p)) propMap =
          bottom -- if any required field has unsatisfiable schema
        | M.null propMap
          , allTop addProps =
          top -- if all fields are optional and have trivial schemata
        | otherwise =
          top
            { forObject = singletonFormula $ Properties propMap addProps addPropSchema
            }

      maxPropertiesClause = case _schemaMaxProperties of
        Nothing -> top
        Just n ->
          top
            { forObject = singletonFormula $ MaxProperties n
            }

      minPropertiesClause = case _schemaMinProperties of
        Nothing -> top
        Just n ->
          top
            { forObject = singletonFormula $ MinProperties n
            }

      nullableClause
        | Just True <- _schemaNullable =
          bottom
            { forNull = singletonFormula $ Exactly TNull
            }
        | otherwise = bottom

  pure $
    nullableClause
      \/ meets
        (allClauses
           <> [ anyClause
              , oneClause
              , typeClause
              , enumClause
              , maximumClause
              , minimumClause
              , multipleOfClause
              , formatClause
              , maxLengthClause
              , minLengthClause
              , patternClause
              , itemsClause
              , maxItemsClause
              , minItemsClause
              , uniqueItemsClause
              , propertiesClause
              , maxPropertiesClause
              , minPropertiesClause
              ])

{- TODO: ReadOnly/WriteOnly #68 -}

checkOneOfDisjoint :: MonadProcess m => [Traced (Referenced Schema)] -> m Bool
checkOneOfDisjoint = const $ pure True -- TODO #69

schemaToFormula
  :: Traced (Definitions Schema)
  -> Traced Schema
  -> (ForeachType JsonFormula, P.PathsPrefixTree Behave AnIssue 'SchemaLevel)
schemaToFormula defs rs = runWriter . (`runReaderT` defs) . runMemo () $ processSchema rs

checkFormulas
  :: (ReassembleHList xs (CheckEnv (Referenced Schema)))
  => HList xs
  -> Behavior 'SchemaLevel
  -> ProdCons (Traced (Definitions Schema))
  -> ProdCons (ForeachType JsonFormula, P.PathsPrefixTree Behave AnIssue 'SchemaLevel)
  -> SemanticCompatFormula ()
checkFormulas env beh defs (ProdCons (fp, ep) (fc, ec)) =
  case P.toList ep ++ P.toList ec of
    issues@(_ : _) -> F.for_ issues $ embedFormula beh . anItem
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
      -- Therefore we have the isomorphisms with (∃ and ∀ being the N-ary
      -- versions of \/ and /\ respectively):
      --   (⋃_i ⋂_j A[i,j]) ⊂ (⋃_k ⋂_l B[k,l])
      --   = ∃k ∀l ∀i, (⋂_j A[i,j]) ⊂ B[k,l]
      --   = ∀i ∃k ∀l, (⋂_j A[i,j]) ⊂ B[k,l]
      -- with the caveat that the the set over which k ranges is nonempty.
      -- This is because 1) is not an isomorphism.
      -- Our disjunction loses information, so it makes sense to nest it as
      -- deeply as possible, hence we choose the latter representation.
      --
      -- We delegate the verification of (⋂_j A[j]) ⊂ B to a separate heuristic
      -- function, with the understanding that ∃j, A[j] ⊂ B is a sufficient,
      -- but not necessary condition (because of 2) and 3)).
      --
      -- If k ranges over an empty set, we have the isomorphism:
      --   (⋃_i ⋂_j A[i,j]) ⊂ ∅ = ∀i, (⋂_j A[i,j]) ⊂ ∅
      -- where we again delegate (⋂_j A[j]) ⊂ ∅ to a heuristic, though here the
      -- shortcut of ∃j, A[j] ⊂ ∅ hardly helps.
      --
      -- Disjunctions tend to erase informative error messages, so we may want
      -- to avoid them. This can be formally done as follows: if we can
      -- partition the universal set into a disjoint union of some parts:
      --   ⊤ = ⊔_α P[α]
      -- such that the conjuncts in our disjunctive normal form are subordinate
      -- to the partition:
      --   ∀i ∃α, (⋂_j A[i,j]) ⊂ P[α]
      --   ∀k ∃α, (⋂_l B[k,l]) ⊂ P[α]
      -- then we can partition the sets over which i and k range into partitions
      -- I[α] and K[α], and then in each "bucket" verify the inclusion in the
      -- aforementioned way:
      --   ∀α, (⋃_i∈I[α] ⋂_j A[i,j]) ⊂ (⋃_k∈K[α] ⋂_l B[k,l])
      --   = ∀α ∀i∈I[α] ∃k∈K[α] ∀l, (⋂_j A[i,j]) ⊂ B[k,l]
      -- We already somewhat do this by partitioning JSON into types, but we can
      -- additionally partition e.g. "enum" fields or existence of particular
      -- properties. This works especially well if we manage to ensure K[α] are
      -- 1-element sets.
      --
      -- Since the set:
      --   (⋃_i∈I[α] ⋂_j A[i,j]) = (⋃_i ⋂_j A[i,j]) ∩ P[α]
      -- does not actually appear in the source schema, we need to construct it
      -- ourselves and come up with a name for it.
      let typesRestricted = not (anyBottomTypes fp) && anyBottomTypes fc
      -- Specifically handle the case when a schema's type has been
      -- restricted from "all" to specific types: if all types were allowed
      -- in the producer and not all types are allowed in the consumer, it's
      -- usually easier to say what's left than what's removed
      when typesRestricted $ issueAt beh $ TypesRestricted $ nonBottomTypes fc
      forType_ $ \tyName ty -> do
        let beh' = beh >>> step (OfType tyName)
        case (getJsonFormula $ ty fp, getJsonFormula $ ty fc) of
          (DNF pss, BottomFormula) -> unless typesRestricted $ do
            -- don't repeat the TypesRestricted issue
            F.for_ pss $ \(Conjunct ps) -> checkContradiction beh' ps
          (DNF pss, SingleConjunct cs) -> F.for_ pss $ \(Conjunct ps) -> do
            F.for_ cs $ checkImplication env beh' ps -- avoid disjunction if there's only one conjunct
          (TopFormula, DNF css) ->
            -- producer is "open" (allows any value), but consumer has restrictions.
            -- In this case we want to show which restrictions were added. (instead
            -- of showing an empty list restrictions that couldn't be satisfied.)
            F.for_ css $ \(Conjunct cs) -> F.for_ cs $ checkImplication env beh' []
          (pss', css') -> F.for_ (tryPartition defs $ ProdCons (JsonFormula pss') (JsonFormula css')) $ \case
            (mPart, ProdCons pf cf) -> do
              let beh'' = foldr ((<<<) . step . InPartition) beh' mPart
              case (getJsonFormula pf, getJsonFormula cf) of
                (DNF pss, BottomFormula) -> F.for_ pss $ \(Conjunct ps) -> checkContradiction beh'' ps
                (DNF pss, SingleConjunct cs) -> F.for_ pss $ \(Conjunct ps) -> do
                  F.for_ cs $ checkImplication env beh'' ps
                -- unlucky:
                (DNF pss, DNF css) -> F.for_ pss $ \(Conjunct ps) -> do
                  anyOfAt
                    beh'
                    (issueFromConjunct Nothing ps)
                    [F.for_ cs $ checkImplication env beh' ps | Conjunct cs <- S.toList css]
      pure ()
  where
    anyBottomTypes f = getAny $
      foldType $ \_ ty -> case getJsonFormula $ ty f of
        BottomFormula -> Any True
        _ -> mempty
    nonBottomTypes f = foldType $ \tyName ty -> case getJsonFormula $ ty f of
      BottomFormula -> mempty
      _ -> [tyName]
    issueFromConjunct :: Typeable t => Partition -> [Condition t] -> Issue 'TypedSchemaLevel
    issueFromConjunct _ ps
      | Just e <- findExactly ps
        , all (satisfiesTyped e) ps =
        EnumDoesntSatisfy $ untypeValue e -- what does this look like when partitioned?
    issueFromConjunct mPart ps = NoMatchingCondition mPart $ SomeCondition <$> ps

data PartitionData
  = DByEnumValue (DNF (S.Set A.Value))
  | DByProperties (DNF (S.Set Text, S.Set Text)) -- optional, required
  deriving stock (Eq, Ord, Show)

data PartitionChoice
  = CByEnumValue (S.Set A.Value)
  | CByProperties (S.Set Text) (S.Set Text) -- included, excluded
  deriving stock (Eq, Ord, Show)

conjPart :: PartitionData -> PartitionData -> Maybe PartitionData
conjPart (DByEnumValue xss) (DByEnumValue yss) = Just . DByEnumValue $ xss /\ yss
conjPart (DByProperties xss) (DByProperties yss) = Just . DByProperties $ xss /\ yss
conjPart _ _ = Nothing

disjPart :: PartitionData -> PartitionData -> Maybe PartitionData
disjPart (DByEnumValue xss) (DByEnumValue yss) = Just . DByEnumValue $ xss \/ yss
disjPart (DByProperties xss) (DByProperties yss) = Just . DByProperties $ xss \/ yss
disjPart _ _ = Nothing

data PartitionLocation
  = PHere
  | PInProperty Text PartitionLocation
  deriving stock (Eq, Ord, Show)

newtype Partitions = Partitions (M.Map PartitionLocation (S.Set PartitionData))
  deriving stock (Eq, Ord, Show)

instance Lattice Partitions where
  Partitions xss /\ Partitions yss = Partitions $ M.unionWith conj xss yss
    where
      conj xs ys = S.fromList . catMaybes $ liftA2 conjPart (S.toList xs) (S.toList ys)
  Partitions xss \/ Partitions yss = Partitions $ M.intersectionWith disj xss yss
    where
      disj xs ys = S.fromList . catMaybes $ liftA2 disjPart (S.toList xs) (S.toList ys)

instance BoundedMeetSemiLattice Partitions where
  top = Partitions M.empty

-- The lattice has no bottom, but we use 'Lifted' to adjoin a free bottom element

type PartitionM = ReaderT (Traced (Definitions Schema)) (State (MemoState ()))

ignoreKnot :: KnotTier (Lifted Partitions) () PartitionM
ignoreKnot =
  KnotTier
    { onKnotFound = pure ()
    , onKnotUsed = \_ -> pure bottom
    , tieKnot = \_ -> pure
    }

singletonPart :: PartitionData -> Lifted Partitions
singletonPart = Lift . Partitions . M.singleton PHere . S.singleton

partitionSchema :: Traced Schema -> PartitionM (Lifted Partitions)
partitionSchema sch = do
  allClauses <- case tracedAllOf sch of
    Nothing -> pure []
    Just xs -> mapM partitionRefSchema xs

  anyClause <- case tracedAnyOf sch of
    Nothing -> pure top
    Just xs -> joins <$> mapM partitionRefSchema xs

  oneClause <- case tracedOneOf sch of
    Nothing -> pure top
    Just xs -> joins <$> mapM partitionRefSchema xs

  byEnumClause <- case _schemaEnum $ extract sch of
    Nothing -> pure top
    Just xs ->
      pure . singletonPart $
        DByEnumValue $ SingleConjunct [S.fromList xs]

  -- We can only partition by presence of a property if additional properties
  -- are disallowed, and the property is not optional
  let reqd = S.fromList $ _schemaRequired $ extract sch
  byPropertiesClause <- case _schemaAdditionalProperties $ extract sch of
    Just (AdditionalPropertiesAllowed False) -> do
      let props = S.fromList . IOHM.keys . _schemaProperties $ extract sch
      pure . singletonPart $
        DByProperties $ SingleConjunct [(props S.\\ reqd, props `S.intersection` reqd)]
    _ -> pure top

  -- We can partition on something nested in a property only if the property is
  -- required
  let reqdProps = IOHM.filterWithKey (\k _ -> k `S.member` reqd) $ tracedProperties sch
  inPropertiesClauses <- forM (IOHM.toList reqdProps) $ \(k, rs) -> do
    f <- partitionRefSchema rs
    pure $ fmap (\(Partitions m) -> Partitions $ M.mapKeysMonotonic (PInProperty k) m) f

  pure $ meets $ allClauses <> [anyClause, oneClause, byEnumClause, byPropertiesClause] <> inPropertiesClauses

partitionRefSchema :: Traced (Referenced Schema) -> PartitionM (Lifted Partitions)
partitionRefSchema x = do
  defs <- R.ask
  memoWithKnot ignoreKnot (partitionSchema $ dereference defs x) (ask x)

newtype LiftA f a = LiftA {getLiftA :: f a}
  deriving newtype (Functor, Applicative)

instance (Lattice a, Applicative f) => Lattice (LiftA f a) where
  (/\) = liftA2 (/\)
  (\/) = liftA2 (\/)

instance (BoundedJoinSemiLattice a, Applicative f) => BoundedJoinSemiLattice (LiftA f a) where
  bottom = pure bottom

instance (BoundedMeetSemiLattice a, Applicative f) => BoundedMeetSemiLattice (LiftA f a) where
  top = pure top

partitionCondition :: Condition t -> PartitionM (Lifted Partitions)
partitionCondition = \case
  Exactly x ->
    pure . singletonPart $
      DByEnumValue $ SingleConjunct [S.singleton $ untypeValue x]
  Properties props _ madd -> do
    let byProps = case madd of
          Just _ -> top
          Nothing ->
            singletonPart $
              DByProperties $
                SingleConjunct
                  [ ( M.keysSet $ M.filter (not . propRequired) props
                    , M.keysSet $ M.filter propRequired props
                    )
                  ]
    inProps <- forM (M.toList $ M.filter propRequired props) $ \(k, prop) -> do
      f <- partitionRefSchema $ propRefSchema prop
      pure $ fmap (\(Partitions m) -> Partitions $ M.mapKeysMonotonic (PInProperty k) m) f
    pure $ byProps /\ meets inProps
  _ -> pure top

partitionJsonFormulas
  :: ProdCons (Traced (Definitions Schema))
  -> ProdCons (JsonFormula t)
  -> Lifted Partitions
partitionJsonFormulas defs pc = producer pcPart \/ consumer pcPart
  where
    pcPart = partitionFormula <$> defs <*> pc
    partitionFormula def (JsonFormula xss) = runIdentity . runMemo () . (`runReaderT` def) $ do
      getLiftA . foldLattice (LiftA . partitionCondition) $ xss

selectPartition :: Lifted Partitions -> Maybe (PartitionLocation, S.Set PartitionChoice)
selectPartition Bottom = Nothing
selectPartition (Lift (Partitions m)) =
  go [(loc, part) | (loc, parts) <- sortBy (comparing $ locLength . fst) $ M.toList m, part <- S.toList parts]
  where
    locLength :: PartitionLocation -> Int
    locLength = walk 0
      where
        walk !n PHere = n
        walk !n (PInProperty _ l) = walk (n + 1) l
    go [] = Nothing
    -- Skip partitioning by property for now
    go ((_, DByProperties _) : ps) = go ps
    -- Don't partition by enum value at the root (this reports removed enum values as contradictions in their respective partitions)
    go ((PHere, DByEnumValue _) : ps) = go ps
    go ((loc, DByEnumValue (DNF xss)) : ps)
      -- Check that no disjunction branches are unresticted
      | Just enums <- traverse (fmap (foldr1 S.intersection) . NE.nonEmpty . S.toList) . S.toList $ xss =
        -- TODO: improve
        Just (loc, S.map (CByEnumValue . S.singleton) $ S.unions enums)
      | otherwise = go ps

-- This essentially has 3 cases:
-- Nothing -- we have produced a bottom schema
-- Just (False, _) -- there's been no change to the schema
-- Just (True, x) -- x is a new schema
type IntersectionM = ReaderT (Traced (Definitions Schema)) (WriterT Any Maybe)

mBottom :: IntersectionM a
mBottom = lift . lift $ Nothing

catchBottom :: IntersectionM a -> IntersectionM a -> IntersectionM a
catchBottom act handler = R.liftCatch (W.liftCatch (\a h -> a <|> h ())) act (\_ -> handler)

mChange :: IntersectionM ()
mChange = tell $ Any True

intersectSchema
  :: PartitionLocation
  -> PartitionChoice
  -> Traced Schema
  -> IntersectionM Schema
intersectSchema loc part sch = do
  allOf' <- forM (tracedAllOf sch) $ \rss ->
    -- Assuming i ranges over a nonempty set (checked in processSchema)
    -- (⋂_i A[i]) ∩ X = ⋂_i (A[i] ∩ X)
    -- If any intersections are empty, the result is empty. If any intersections are a change, the result is a change.
    traverse (intersectRefSchema loc part) rss
  anyOf' <- forM (tracedAnyOf sch) $ \rss -> do
    -- (⋃_i A[i]) ∩ X = ⋃_i (A[i] ∩ X)
    -- Collect only the nonempty A[i] ∩ X, unless there are none, in which case the result is empty.
    -- If any schema is empty, we remove it from the list which constitutes a change.
    mSchemas <- forM rss $ \rs -> catchBottom (Just <$> intersectRefSchema loc part rs) (mChange >> pure Nothing)
    case catMaybes mSchemas of
      [] -> mBottom
      schs -> pure schs
  oneOf' <- forM (tracedOneOf sch) $ \rss -> do
    -- Same as anyOf'. By intersecting we're only making them more disjoint if anything.
    mSchemas <- forM rss $ \rs -> catchBottom (Just <$> intersectRefSchema loc part rs) (mChange >> pure Nothing)
    case catMaybes mSchemas of
      [] -> mBottom
      schs -> pure schs
  let sch' = (extract sch) {_schemaAllOf = allOf', _schemaAnyOf = anyOf', _schemaOneOf = oneOf'}
  -- Now the local changes:
  case loc of
    PInProperty k loc' -> case IOHM.lookup k $ tracedProperties sch of
      Nothing -> error $ "Partitioning via absent property: " <> T.unpack k
      Just prop -> do
        prop' <- intersectRefSchema loc' part prop
        pure $ sch' {_schemaProperties = IOHM.adjust (const prop') k $ _schemaProperties sch'}
    PHere -> case part of
      CByEnumValue vals -> do
        enum' <- case _schemaEnum sch' of
          Nothing -> do
            mChange
            pure $ S.toList vals
          Just xs -> do
            when (any (`S.notMember` vals) xs) mChange
            case filter (`S.member` vals) xs of
              [] -> mBottom
              xs' -> pure xs'
        pure $ sch' {_schemaEnum = Just enum'}
      CByProperties {} -> error "CByProperties not implemented"

intersectRefSchema
  :: PartitionLocation
  -> PartitionChoice
  -> Traced (Referenced Schema)
  -> IntersectionM (Referenced Schema)
intersectRefSchema loc part rs = do
  defs <- R.ask
  Inline <$> intersectSchema loc part (dereference defs rs)

intersectCondition :: Traced (Definitions Schema) -> PartitionLocation -> PartitionChoice -> Condition t -> DNF (Condition t)
intersectCondition _defs PHere (CByEnumValue values) cond@(Exactly x) =
  if untypeValue x `S.member` values then SingleConjunct [cond] else bottom
intersectCondition defs (PInProperty k loc) part cond@(Properties props add madd) = case M.lookup k props of
  Nothing -> SingleConjunct [cond] -- shouldn't happen
  Just prop -> case runWriterT . (`runReaderT` defs) $ intersectRefSchema loc part $ propRefSchema prop of
    Just (rs', Any True) ->
      let trs' = traced (ask (propRefSchema prop) >>> step (Partitioned loc part)) rs'
       in SingleConjunct [Properties (M.insert k prop {propRefSchema = trs'} props) add madd]
    Just (_, Any False) -> SingleConjunct [cond]
    Nothing -> bottom
intersectCondition _defs _loc _part cond = SingleConjunct [cond]

intersectFormula :: Traced (Definitions Schema) -> PartitionLocation -> PartitionChoice -> JsonFormula t -> JsonFormula t
intersectFormula defs loc part = JsonFormula . foldLattice (intersectCondition defs loc part) . getJsonFormula

type Partition = Maybe (PartitionLocation, PartitionChoice)

tryPartition :: ProdCons (Traced (Definitions Schema)) -> ProdCons (JsonFormula t) -> [(Partition, ProdCons (JsonFormula t))]
tryPartition defs pc = case selectPartition $ partitionJsonFormulas defs pc of
  Nothing -> [(Nothing, pc)]
  Just (loc, parts) -> [(Just (loc, part), intersectFormula <$> defs <*> pure loc <*> pure part <*> pc) | part <- S.toList parts]

checkContradiction
  :: Behavior 'TypedSchemaLevel
  -> [Condition t]
  -> SemanticCompatFormula ()
checkContradiction beh _ = issueAt beh NoContradiction -- TODO #70

tracedConjunct :: NE.NonEmpty (Traced (Referenced Schema)) -> Traced (Referenced Schema)
tracedConjunct refSchemas = case NE.sortWith ask refSchemas of
  (rs NE.:| []) -> rs
  (rs1 NE.:| rs2 : rss) ->
    traced (ask rs1 >>> step (ConjunctedWith $ ask <$> rs2 NE.:| rss)) $
      Inline mempty {_schemaAllOf = Just $ extract <$> rs1 : rs2 : rss}

checkImplication
  :: (ReassembleHList xs (CheckEnv (Referenced Schema)))
  => HList xs
  -> Behavior 'TypedSchemaLevel
  -> [Condition t]
  -> Condition t
  -> SemanticCompatFormula ()
checkImplication env beh prods cons = case findExactly prods of
  Just e
    | all (satisfiesTyped e) prods ->
      if satisfiesTyped e cons
        then pure ()
        else issueAt beh (EnumDoesntSatisfy $ untypeValue e)
    | otherwise -> pure () -- vacuously true
  Nothing -> case cons of
    -- the above code didn't catch it, so there's no Exactly condition on the lhs
    Exactly e -> issueAt beh (NoMatchingEnum $ untypeValue e)
    Maximum m -> case findRelevant min (\case Maximum m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' <= m
          then pure ()
          else issueAt beh (MatchingMaximumWeak $ ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMaximum m)
    Minimum m -> case findRelevant max (\case Minimum m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' >= m
          then pure ()
          else issueAt beh (MatchingMinimumWeak ProdCons {producer = coerce m', consumer = coerce m})
      Nothing -> issueAt beh (NoMatchingMinimum (coerce m))
    MultipleOf m -> case findRelevant lcmScientific (\case MultipleOf m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if lcmScientific m m' == m'
          then pure ()
          else issueAt beh (MatchingMultipleOfWeak $ ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMultipleOf m)
    NumberFormat f ->
      if any (\case NumberFormat f' -> f == f'; _ -> False) prods
        then pure ()
        else issueAt beh (NoMatchingFormat f)
    MaxLength m -> case findRelevant min (\case MaxLength m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' <= m
          then pure ()
          else issueAt beh (MatchingMaxLengthWeak $ ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMaxLength m)
    MinLength m -> case findRelevant max (\case MinLength m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' >= m
          then pure ()
          else issueAt beh (MatchingMinLengthWeak $ ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMinLength m)
    Pattern p ->
      if any (\case Pattern p' -> p == p'; _ -> False) prods
        then pure ()
        else issueAt beh (NoMatchingPattern p) -- TODO: regex comparison #32
    StringFormat f ->
      if any (\case StringFormat f' -> f == f'; _ -> False) prods
        then pure ()
        else issueAt beh (NoMatchingFormat f)
    Items _ cons' -> case findRelevant (<>) (\case Items _ rs -> Just (rs NE.:| []); _ -> Nothing) prods of
      Just (tracedConjunct -> rs) -> checkCompatibility (beh >>> step InItems) env $ ProdCons rs cons'
      Nothing -> issueAt beh NoMatchingItems
    MaxItems m -> case findRelevant min (\case MaxItems m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' <= m
          then pure ()
          else issueAt beh (MatchingMaxItemsWeak ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMaxItems m)
    MinItems m -> case findRelevant max (\case MinItems m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' >= m
          then pure ()
          else issueAt beh (MatchingMinItemsWeak ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMinItems m)
    UniqueItems ->
      if any (== UniqueItems) $ prods
        then pure ()
        else issueAt beh NoMatchingUniqueItems
    Properties props _ madd -> case findRelevant (<>) (\case Properties props' _ madd' -> Just $ (props', madd') NE.:| []; _ -> Nothing) prods of
      Just pm ->
        anyOfAt beh NoMatchingProperties $
          NE.toList pm <&> \(props', madd') -> do
            F.for_ (S.fromList $ M.keys props <> M.keys props') $ \k -> do
              let beh' = beh >>> step (InProperty k)
                  go sch sch' = checkCompatibility beh' env (ProdCons sch sch')
              case (maybe False propRequired $ M.lookup k props', maybe False propRequired $ M.lookup k props) of
                -- producer does not require field, but consumer does (can fail)
                (False, True) -> issueAt beh' PropertyNowRequired
                _ -> pure ()
              case (M.lookup k props', madd', M.lookup k props, madd) of
                -- (producer, additional producer, consumer, additional consumer)
                (Nothing, Nothing, _, _) -> pure () -- vacuously: the producer asserts that this field cannot exist,
                -- and the consumer either doesn't require it, or it does and we've already raised an error about it.
                (_, _, Nothing, Nothing) -> issueAt beh' UnexpectedProperty
                (Just p', _, Just p, _) -> go (propRefSchema p') (propRefSchema p)
                (Nothing, Just add', Just p, _) ->
                  clarifyIssue (AnItem beh' (anIssue AdditionalToProperty)) $
                    go add' (propRefSchema p)
                (Just p', _, Nothing, Just add) ->
                  clarifyIssue (AnItem beh' (anIssue PropertyToAdditional)) $
                    go (propRefSchema p') add
                (Nothing, Just _, Nothing, Just _) -> pure ()
              pure ()
            case (madd', madd) of
              (Nothing, _) -> pure () -- vacuously
              (_, Nothing) -> issueAt beh NoAdditionalProperties
              (Just add', Just add) -> checkCompatibility (beh >>> step InAdditionalProperty) env (ProdCons add' add)
            pure ()
      Nothing -> issueAt beh NoMatchingProperties
    MaxProperties m -> case findRelevant min (\case MaxProperties m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' <= m
          then pure ()
          else issueAt beh (MatchingMaxPropertiesWeak ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMaxProperties m)
    MinProperties m -> case findRelevant max (\case MinProperties m' -> Just m'; _ -> Nothing) prods of
      Just m' ->
        if m' >= m
          then pure ()
          else issueAt beh (MatchingMinPropertiesWeak ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (NoMatchingMinProperties m)
  where
    findRelevant combine extr =
      fmap (foldr1 combine) . NE.nonEmpty . mapMaybe extr
    lcmScientific (toRational -> a) (toRational -> b) =
      fromRational $ lcm (numerator a) (numerator b) % gcd (denominator a) (denominator b)

findExactly :: [Condition t] -> Maybe (TypedValue t)
findExactly (Exactly x : _) = Just x
findExactly (_ : xs) = findExactly xs
findExactly [] = Nothing

instance Issuable 'TypedSchemaLevel where
  data Issue 'TypedSchemaLevel
    = -- | producer produces a specific value ($1), consumer has a condition that is not satisfied by said value
      EnumDoesntSatisfy A.Value
    | -- | consumer only expects a specific value which the producer does not produce.
      NoMatchingEnum A.Value
    | -- | consumer declares a maximum numeric value ($1), producer doesn't
      NoMatchingMaximum (Bound Scientific)
    | -- | consumer declares a maximum numeric value ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaximumWeak (ProdCons (Bound Scientific))
    | -- | consumer declares a minimum numeric value, producer doesn't
      NoMatchingMinimum (Bound Scientific)
    | -- | consumer declares a minimum numeric value ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinimumWeak (ProdCons (Bound Scientific))
    | -- | consumer declares that the numeric value must be a multiple of $1, producer doesn't
      NoMatchingMultipleOf Scientific
    | -- | consumer declares that the numeric value must be a multiple of $1, producer declares a weaker condition (multiple of $2)
      MatchingMultipleOfWeak (ProdCons Scientific)
    | -- | consumer declares a string/number format, producer declares none or a different format (TODO: improve via regex #32)
      NoMatchingFormat Format
    | -- | consumer declares a maximum length of the string ($1), producer doesn't.
      NoMatchingMaxLength Integer
    | -- | consumer declares a maximum length of the string ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxLengthWeak (ProdCons Integer)
    | -- | consumer declares a minimum length of the string ($1), producer doesn't.
      NoMatchingMinLength Integer
    | -- | consumer declares a minimum length of the string ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinLengthWeak (ProdCons Integer)
    | -- | consumer declares the string value must match a regex ($1), producer doesn't declare or declares different regex (TODO: #32)
      NoMatchingPattern Pattern
    | -- | consumer declares the items of an array must satisfy some condition, producer doesn't
      NoMatchingItems
    | -- | consumer declares a maximum length of the array ($1), producer doesn't.
      NoMatchingMaxItems Integer
    | -- | consumer declares a maximum length of the array ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxItemsWeak (ProdCons Integer)
    | -- | consumer declares a minimum length of the array ($1), producer doesn't.
      NoMatchingMinItems Integer
    | -- | consumer declares a minimum length of the array ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinItemsWeak (ProdCons Integer)
    | -- | consumer declares that items must be unique, producer doesn't
      NoMatchingUniqueItems
    | -- | consumer declares the properties of an object must satisfy some condition, producer doesn't
      NoMatchingProperties
    | -- | producer allows additional properties, consumer doesn't
      NoAdditionalProperties
    | -- | consumer declares a maximum number of properties in the object ($1), producer doesn't.
      NoMatchingMaxProperties Integer
    | -- | consumer declares a maximum number of properties in the object ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxPropertiesWeak (ProdCons Integer)
    | -- | consumer declares a minimum number of properties in the object ($1), producer doesn't.
      NoMatchingMinProperties Integer
    | -- | consumer declares a minimum number of properties in the object ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinPropertiesWeak (ProdCons Integer)
    | -- | producer declares that the value must satisfy a disjunction of some conditions, but consumer's requirements couldn't be matched against any single one of them (TODO: split heuristic #71)
      NoMatchingCondition Partition [SomeCondition]
    | -- | producer indicates that values of this type are now allowed, but the consumer does not do so (currently we only check immediate contradictions, c.f. #70)
      -- AKA consumer does not have the type
      NoContradiction
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False
  describeIssue Forward (EnumDoesntSatisfy v) = para "The following enum value was removed:" <> showJSONValue v
  describeIssue Backward (EnumDoesntSatisfy v) = para "The following enum value was added:" <> showJSONValue v
  describeIssue Forward (NoMatchingEnum v) = para "The following enum value has been added:" <> showJSONValue v
  describeIssue Backward (NoMatchingEnum v) = para "The following enum value has been removed:" <> showJSONValue v
  describeIssue Forward (NoMatchingMaximum b) = para $ "Upper bound has been added:" <> showBound b <> "."
  describeIssue Backward (NoMatchingMaximum b) = para $ "Upper bound has been removed:" <> showBound b <> "."
  describeIssue _ (MatchingMaximumWeak (ProdCons p c)) = para $ "Upper bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMinimum b) = para $ "Lower bound has been added: " <> showBound b <> "."
  describeIssue Backward (NoMatchingMinimum b) = para $ "Lower bound has been removed: " <> showBound b <> "."
  describeIssue _ (MatchingMinimumWeak (ProdCons p c)) = para $ "Lower bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMultipleOf n) = para $ "Value is now a multiple of " <> show' n <> "."
  describeIssue Backward (NoMatchingMultipleOf n) = para $ "Value is no longer a multiple of " <> show' n <> "."
  describeIssue _ (MatchingMultipleOfWeak (ProdCons p c)) = para $ "Value changed from being a multiple of " <> show' p <> " to being a multiple of " <> show' c <> "."
  describeIssue Forward (NoMatchingFormat f) = para $ "Format added: " <> code f <> "."
  describeIssue Backward (NoMatchingFormat f) = para $ "Format removed: " <> code f <> "."
  describeIssue Forward (NoMatchingMaxLength n) = para $ "Maximum length added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxLength n) = para $ "Maximum length removed: " <> show' n <> "."
  describeIssue _ (MatchingMaxLengthWeak (ProdCons p c)) = para $ "Maximum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinLength n) = para $ "Minimum length of the string added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinLength n) = para $ "Minimum length of the string removed: " <> show' n <> "."
  describeIssue _ (MatchingMinLengthWeak (ProdCons p c)) = para $ "Minimum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingPattern p) = para "Pattern (regular expression) added: " <> codeBlock p
  describeIssue Backward (NoMatchingPattern p) = para "Pattern (regular expression) removed: " <> codeBlock p
  describeIssue Forward NoMatchingItems = para "Array item schema has been added."
  describeIssue Backward NoMatchingItems = para "Array item schema has been removed."
  describeIssue Forward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been added " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been removed " <> show' n <> "."
  describeIssue _ (MatchingMaxItemsWeak (ProdCons p c)) = para $ "Maximum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinItems n) = para $ "Minimum length of the array added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinItems n) = para $ "Minimum length of the array removed: " <> show' n <> "."
  describeIssue _ (MatchingMinItemsWeak (ProdCons p c)) = para $ "Minimum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward NoMatchingUniqueItems = para "Items are now required to be unique."
  describeIssue Backward NoMatchingUniqueItems = para "Items are no longer required to be unique."
  describeIssue Forward NoMatchingProperties = para "Property added."
  describeIssue Backward NoMatchingProperties = para "Property removed."
  describeIssue Forward NoAdditionalProperties = para "Additional properties have been removed."
  describeIssue Backward NoAdditionalProperties = para "Additional properties have been added."
  describeIssue Forward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been removed: " <> show' n <> "."
  describeIssue _ (MatchingMaxPropertiesWeak (ProdCons p c)) = para $ "Maximum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinProperties n) = para $ "Minimum number of properties added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinProperties n) = para $ "Minimum number of properties removed: " <> show' n <> "."
  describeIssue _ (MatchingMinPropertiesWeak (ProdCons p c)) = para $ "Minimum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue _ (NoMatchingCondition mPart conds) =
    para
      (case mPart of
         Nothing -> "Could not verify that the following conditions hold (please file a bug if you see this)"
         Just locPart ->
           showPartition locPart
             <> " – could not verify that the following conditions hold (please file a bug if you see this):")
      <> bulletList ((\(SomeCondition c) -> showCondition c) <$> conds)
  describeIssue Forward NoContradiction = para "The value has been removed."
  describeIssue Backward NoContradiction = para "The value has been added."

showJSONValue :: A.Value -> Blocks
showJSONValue v = codeBlockWith ("", ["json"], mempty) (T.decodeUtf8 . BSL.toStrict . A.encode $ v)

showJSONValueInline :: A.Value -> Inlines
showJSONValueInline v = code (T.decodeUtf8 . BSL.toStrict . A.encode $ v)

showBound :: Show a => Bound a -> Inlines
showBound (Inclusive x) = show' x <> " inclusive"
showBound (Exclusive x) = show' x <> " exclusive"

show' :: Show x => x -> Inlines
show' = str . T.pack . show

instance Issuable 'SchemaLevel where
  data Issue 'SchemaLevel
    = -- | Some (openapi-supported) feature that we do not support was encountered in the schema
      NotSupported Text
    | -- | The schema is actually invalid
      InvalidSchema Text
    | -- | The schema contains a reference loop along "anyOf"/"allOf"/"oneOf".
      UnguardedRecursion
    | -- | Producer doesn't place any restrictions on the types, but the consumer does. List what types remain available in the consumer.
      TypesRestricted [JsonType]
    | -- | in the producer this field used to be handled as part of "additionalProperties", and the consumer this is a specific "properties" entry. Only thrown when this change actually causes other issues
      AdditionalToProperty
    | -- | in the consumer this field used to be handled as part of "additionalProperties", and the producer this is a specific "properties" entry. Only thrown when this change actually causes other issues
      PropertyToAdditional
    | -- | consumer requires a property that is not required/allowed in the producer
      PropertyNowRequired
    | -- | producer allows a property that is not allowed in the consumer
      UnexpectedProperty
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported = \case
    NotSupported _ -> True
    InvalidSchema _ -> True
    UnguardedRecursion -> True
    _ -> False

  describeIssue _ (NotSupported i) =
    para (emph "Encountered a feature that OpenApi Diff does not support: " <> text i <> ".")
  describeIssue _ (InvalidSchema i) =
    para (emph "The schema is invalid: " <> text i <> ".")
  describeIssue _ UnguardedRecursion =
    para "Encountered recursion that is too complex for OpenApi Diff to untangle."
  describeIssue Forward (TypesRestricted tys) = case tys of
    [] -> para "No longer has any valid values." -- weird
    _ -> para "Values are now limited to the following types: " <> bulletList (para . describeJSONType <$> tys)
  describeIssue Backward (TypesRestricted tys) = case tys of
    [] -> para "Any value of any type is now allowed." -- weird
    _ -> para "Values are no longer limited to the following types: " <> bulletList (para . describeJSONType <$> tys)
  describeIssue Forward AdditionalToProperty = para "The property was previously implicitly described by the catch-all \"additional properties\" case. It is now explicitly defined."
  describeIssue Backward AdditionalToProperty = para "The property was previously explicitly defined. It is now implicitly described by the catch-all \"additional properties\" case."
  describeIssue Forward PropertyToAdditional = para "The property was previously explicitly defined. It is now implicitly described by the catch-all \"additional properties\" case."
  describeIssue Backward PropertyToAdditional = para "The property was previously implicitly described by the catch-all \"additional properties\" case. It is now explicitly defined."
  describeIssue Forward PropertyNowRequired = para "The property has become required."
  describeIssue Backward PropertyNowRequired = para "The property may not be present."
  describeIssue Forward UnexpectedProperty = para "The property has been removed."
  describeIssue Backward UnexpectedProperty = para "The property has been added."

instance Behavable 'SchemaLevel 'TypedSchemaLevel where
  data Behave 'SchemaLevel 'TypedSchemaLevel
    = OfType JsonType
    deriving stock (Eq, Ord, Show)

  describeBehaviour (OfType t) = describeJSONType t

describeJSONType :: IsString s => JsonType -> s
describeJSONType = \case
  Null -> "Null"
  Boolean -> "Boolean"
  Number -> "Number"
  String -> "String"
  Array -> "Array"
  Object -> "Object"

instance Behavable 'TypedSchemaLevel 'TypedSchemaLevel where
  data Behave 'TypedSchemaLevel 'TypedSchemaLevel
    = InPartition (PartitionLocation, PartitionChoice)
    deriving stock (Eq, Ord, Show)

  describeBehaviour (InPartition partition) = showPartition partition

showPartition :: (PartitionLocation, PartitionChoice) -> Inlines
showPartition = \case
  (partition, CByEnumValue (S.toList -> [v])) ->
    "In cases where " <> renderPartitionLocation partition <> " is " <> showJSONValueInline v
  (partition, CByEnumValue (S.toList -> vs)) ->
    "In cases where " <> renderPartitionLocation partition <> " has values: "
      <> (fold . L.intersperse ", " . fmap showJSONValueInline $ vs)
  (partition, CByProperties (S.toList -> incl) (S.toList -> [])) ->
    "In cases where " <> renderPartitionLocation partition <> " contains the properties: " <> listCodes incl
  (partition, CByProperties (S.toList -> []) (S.toList -> excl)) ->
    "In cases where " <> renderPartitionLocation partition <> " does not contain the properties: " <> listCodes excl
  (partition, CByProperties (S.toList -> incl) (S.toList -> excl)) ->
    "In cases where " <> renderPartitionLocation partition
      <> " contains the properties "
      <> listCodes incl
      <> " and does not contain the properties "
      <> listCodes excl
  where
    listCodes :: [Text] -> Inlines
    listCodes = fold . L.intersperse ", " . fmap code
    renderPartitionLocation :: PartitionLocation -> Inlines
    renderPartitionLocation p = code $ "$" <> renderPartitionLocation' p
      where
        renderPartitionLocation' :: PartitionLocation -> Text
        renderPartitionLocation' PHere = mempty
        renderPartitionLocation' (PInProperty prop rest) = "." <> prop <> renderPartitionLocation' rest

instance Behavable 'TypedSchemaLevel 'SchemaLevel where
  data Behave 'TypedSchemaLevel 'SchemaLevel
    = InItems
    | InProperty Text
    | InAdditionalProperty
    deriving stock (Eq, Ord, Show)

  describeBehaviour InItems = "Items"
  describeBehaviour (InProperty p) = "Property " <> code p
  describeBehaviour InAdditionalProperty = "Additional properties"

instance Subtree Schema where
  type SubtreeLevel Schema = 'SchemaLevel
  type CheckEnv Schema = '[ProdCons (Traced (Definitions Schema))]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _schemaRequired <$> pc
    structuralEq $ fmap _schemaNullable <$> pc
    structuralMaybeWith (structuralList env) $ tracedAllOf <$> pc
    structuralMaybeWith (structuralList env) $ tracedOneOf <$> pc
    structuralMaybe env $ sequence . stepTraced NotStep . fmap _schemaNot <$> pc
    structuralMaybeWith (structuralList env) $ tracedAnyOf <$> pc
    iohmStructural env $ stepTraced PropertiesStep . fmap _schemaProperties <$> pc
    structuralMaybeWith structuralAdditionalProperties $ tracedAdditionalProperties <$> pc
    structuralMaybeWith structuralDiscriminator $ tracedDiscriminator <$> pc
    structuralEq $ fmap _schemaReadOnly <$> pc
    structuralEq $ fmap _schemaWriteOnly <$> pc
    structuralEq $ fmap _schemaXml <$> pc
    structuralEq $ fmap _schemaMaxProperties <$> pc
    structuralEq $ fmap _schemaMinProperties <$> pc
    structuralEq $ fmap _schemaDefault <$> pc
    structuralEq $ fmap _schemaType <$> pc
    structuralEq $ fmap _schemaFormat <$> pc
    structuralMaybeWith structuralItems $ tracedItems <$> pc
    structuralEq $ fmap _schemaMaximum <$> pc
    structuralEq $ fmap _schemaExclusiveMaximum <$> pc
    structuralEq $ fmap _schemaMinimum <$> pc
    structuralEq $ fmap _schemaExclusiveMinimum <$> pc
    structuralEq $ fmap _schemaMaxLength <$> pc
    structuralEq $ fmap _schemaMinLength <$> pc
    structuralEq $ fmap _schemaPattern <$> pc
    structuralEq $ fmap _schemaMaxItems <$> pc
    structuralEq $ fmap _schemaMinItems <$> pc
    structuralEq $ fmap _schemaUniqueItems <$> pc
    structuralEq $ fmap _schemaEnum <$> pc
    structuralEq $ fmap _schemaMultipleOf <$> pc
    pure ()
    where
      structuralAdditionalProperties
        (ProdCons (Left x) (Left y)) = unless (x == y) structuralIssue
      structuralAdditionalProperties
        (ProdCons (Right x) (Right y)) =
          checkSubstructure env $ ProdCons x y
      structuralAdditionalProperties _ = structuralIssue
      structuralDiscriminator pc' = do
        structuralEq $ fmap _discriminatorPropertyName <$> pc'
        iohmStructural env $
          stepTraced DiscriminatorMapping . fmap (fmap parseDiscriminatorValue . _discriminatorMapping) <$> pc'
        pure ()
      structuralItems (ProdCons (Left a) (Left b)) =
        checkSubstructure env $ ProdCons a b
      structuralItems (ProdCons (Right a) (Right b)) =
        structuralList env $ ProdCons a b
      structuralItems _ = structuralIssue
  checkSemanticCompatibility env beh schs = do
    let defs = getH env
    checkFormulas env beh defs $ schemaToFormula <$> defs <*> schs
