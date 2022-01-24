module Data.OpenApi.Compare.Validate.Schema.JsonFormula
  ( Bound (..),
    showBound,
    Property (..),
    Condition (..),
    showCondition,
    satisfiesTyped,
    checkStringFormat,
    checkNumberFormat,
    SomeCondition (..),
    JsonFormula (..),
    satisfiesFormula,
    satisfies,
    showJSONValue,
    showJSONValueInline,
  )
where

import Algebra.Lattice
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Kind
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Orphans ()
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema.DNF
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Text.Pandoc.Builder hiding (Format, Null)
import Text.Regex.Pcre2

data Bound a = Exclusive !a | Inclusive !a
  deriving stock (Eq, Show, Functor)

-- | The order is lexicographical on @a * Bool@.
instance Ord a => Ord (Bound a) where
  Exclusive a `compare` Exclusive b = compare a b
  Exclusive a `compare` Inclusive b = if a <= b then LT else GT
  Inclusive a `compare` Exclusive b = if a < b then LT else GT
  Inclusive a `compare` Inclusive b = compare a b

showBound :: Show a => Bound a -> Inlines
showBound (Inclusive x) = show' x <> " inclusive"
showBound (Exclusive x) = show' x <> " exclusive"

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
  Minimum ::
    !(Down (Bound (Down Scientific))) ->
    -- | this has the right Ord
    Condition 'Number
  MultipleOf :: !Scientific -> Condition 'Number
  NumberFormat :: !Format -> Condition 'Number
  MaxLength :: !Integer -> Condition 'String
  MinLength :: !Integer -> Condition 'String
  Pattern :: !Pattern -> Condition 'String
  StringFormat :: !Format -> Condition 'String
  Items ::
    !(ForeachType JsonFormula) ->
    !(Traced (Referenced Schema)) ->
    Condition 'Array
  TupleItems ::
    ![(ForeachType JsonFormula, Traced (Referenced Schema))] ->
    Condition 'Array
  MaxItems :: !Integer -> Condition 'Array
  MinItems :: !Integer -> Condition 'Array
  UniqueItems :: Condition 'Array
  Properties ::
    !(M.Map Text Property) ->
    -- | formula for additional properties
    !(ForeachType JsonFormula) ->
    -- | schema for additional properties, Nothing means bottom
    !(Maybe (Traced (Referenced Schema))) ->
    Condition 'Object
  MaxProperties :: !Integer -> Condition 'Object
  MinProperties :: !Integer -> Condition 'Object

deriving stock instance Eq (Condition t)

deriving stock instance Ord (Condition t)

deriving stock instance Show (Condition t)

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
  (TupleItems is) -> para ("There should be " <> show' (length is) <> " items in the array:") <> bulletList (showForEachJsonFormula . fst <$> is)
  (MaxItems n) -> para $ "The length of the array should be less than or equal to " <> show' n <> "."
  (MinItems n) -> para $ "The length of the array should be more than or equal to " <> show' n <> "."
  UniqueItems -> para "The elements in the array should be unique."
  (Properties props additional _) ->
    bulletList $
      ( M.toList props
          <&> ( \(k, p) ->
                  para (code k)
                    <> para (strong $ if propRequired p then "Required" else "Optional")
                    <> showForEachJsonFormula (propFormula p)
              )
      )
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
          ( \t f -> case getJsonFormula $ f i of
              BottomDNF -> mempty
              (DNF conds) ->
                [ para (describeJSONType t)
                    <> bulletList
                      ( S.toList conds <&> \case
                          Disjunct (S.toList -> []) -> para "Empty"
                          Disjunct (S.toList -> cond) -> bulletList (showCondition <$> cond)
                      )
                ]
          )

showJSONValue :: A.Value -> Blocks
showJSONValue v = codeBlockWith ("", ["json"], mempty) (T.decodeUtf8 . BSL.toStrict . A.encode $ v)

showJSONValueInline :: A.Value -> Inlines
showJSONValueInline v = code (T.decodeUtf8 . BSL.toStrict . A.encode $ v)

show' :: Show x => x -> Inlines
show' = str . T.pack . show

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
satisfiesTyped (TArray a) (TupleItems fs) = length fs == F.length a && and (zipWith satisfies (F.toList a) (fst <$> fs))
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

newtype JsonFormula t = JsonFormula {getJsonFormula :: DNF (Condition t)}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice)

satisfiesFormula :: TypedValue t -> JsonFormula t -> Bool
satisfiesFormula val = foldDNF (satisfiesTyped val) . getJsonFormula

satisfies :: A.Value -> ForeachType JsonFormula -> Bool
satisfies val p = case val of
  A.Null -> satisfiesFormula TNull $ forNull p
  A.Bool b -> satisfiesFormula (TBool b) $ forBoolean p
  A.Number n -> satisfiesFormula (TNumber n) $ forNumber p
  A.String s -> satisfiesFormula (TString s) $ forString p
  A.Array a -> satisfiesFormula (TArray a) $ forArray p
  A.Object o -> satisfiesFormula (TObject o) $ forObject p
