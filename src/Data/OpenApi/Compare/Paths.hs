-- | Utilities for traversing heterogeneous trees. A heterogeneous tree is a
-- collection of datatypes that "contain" eachother in some form of tree
-- structure.
module Data.OpenApi.Compare.Paths
  ( NiceQuiver,
    AdditionalQuiverConstraints,
    Paths (..),
    DiffPaths (..),
    catDiffPaths,
    AnItem (..),
    step,

    -- * Reexports
    (>>>),
    (<<<),
  )
where

import Control.Category
import Data.Kind
import Data.Type.Equality
import Type.Reflection
import Prelude hiding ((.))

type NiceQuiver (q :: k -> j -> Type) (a :: k) (b :: j) =
  (Typeable q, Typeable a, Typeable b, Ord (q a b), Show (q a b), AdditionalQuiverConstraints q a b)

type family AdditionalQuiverConstraints (q :: k -> j -> Type) (a :: k) (b :: j) :: Constraint

-- | All the possible ways to navigate between nodes in a heterogeneous tree
-- form a quiver. The hom-sets of the free category constructed from this quiver
-- are the sets of various multi-step paths between nodes. This is similar to a
-- list, but indexed. The list is in reverse because in practice we append
-- items at the end one at a time.
data Paths (q :: k -> k -> Type) (a :: k) (b :: k) where
  Root :: Paths q a a
  Snoc :: NiceQuiver q b c => Paths q a b -> !(q b c) -> Paths q a c

infixl 5 `Snoc`

deriving stock instance Show (Paths q a b)

step :: NiceQuiver q a b => q a b -> Paths q a b
step s = Root `Snoc` s

instance Category (Paths q) where
  id = Root
  Root . xs = xs
  (Snoc ys y) . xs = Snoc (ys . xs) y

typeRepRHS :: Typeable b => Paths q a b -> TypeRep b
typeRepRHS _ = typeRep

typeRepLHS :: Typeable b => Paths q a b -> TypeRep a
typeRepLHS Root = typeRep
typeRepLHS (Snoc xs _) = typeRepLHS xs

instance TestEquality (Paths q a) where
  testEquality Root Root = Just Refl
  testEquality Root (Snoc ys _) = testEquality (typeRepLHS ys) typeRep
  testEquality (Snoc xs _) Root = testEquality typeRep (typeRepLHS xs)
  testEquality (Snoc _ _) (Snoc _ _) = testEquality typeRep typeRep

instance Eq (Paths q a b) where
  Root == Root = True
  Snoc xs x == Snoc ys y
    | Just Refl <- testEquality (typeRepRHS xs) (typeRepRHS ys) =
      xs == ys && x == y
  _ == _ = False

instance Ord (Paths q a b) where
  compare Root Root = EQ
  compare Root (Snoc _ _) = LT
  compare (Snoc _ _) Root = GT
  compare (Snoc xs x) (Snoc ys y) =
    case testEquality (typeRepRHS xs) (typeRepRHS ys) of
      Just Refl -> compare xs ys <> compare x y
      Nothing -> compare (someTypeRep xs) (someTypeRep ys)

-- | Like a differece list, but indexed.
newtype DiffPaths (q :: k -> k -> Type) (a :: k) (b :: k)
  = DiffPaths (forall c. Paths q c a -> Paths q c b)

catDiffPaths :: DiffPaths q a b -> DiffPaths q b c -> DiffPaths q a c
catDiffPaths (DiffPaths f) (DiffPaths g) = DiffPaths (g . f)

-- _DiffPaths :: Iso (DiffPaths q a b) (DiffPaths q c d) (Paths q a b) (Paths q c d)
-- _DiffPaths = dimap (\(DiffPaths f) -> f Root) $
--   fmap $ \xs -> DiffPaths (>>> xs)

-- | An item related to some path relative to the root @r@.
data AnItem (q :: k -> k -> Type) (f :: k -> Type) (r :: k) where
  AnItem :: Ord (f a) => Paths q r a -> !(f a) -> AnItem q f r

-- the Ord is yuck but we need it and it should be fine in monomorphic cases

instance Eq (AnItem q f r) where
  AnItem xs fx == AnItem ys fy
    | Just Refl <- testEquality xs ys =
      xs == ys && fx == fy
  _ == _ = False

instance Typeable r => Ord (AnItem q f r) where
  compare (AnItem xs fx) (AnItem ys fy) =
    case testEquality xs ys of
      Just Refl -> compare xs ys <> compare fx fy
      Nothing -> case xs of
        Root -> case ys of
          Root -> compare (someTypeRep xs) (someTypeRep ys)
          Snoc _ _ -> compare (someTypeRep xs) (someTypeRep ys)
        Snoc _ _ -> case ys of
          Root -> compare (someTypeRep xs) (someTypeRep ys)
          Snoc _ _ -> compare (someTypeRep xs) (someTypeRep ys)
