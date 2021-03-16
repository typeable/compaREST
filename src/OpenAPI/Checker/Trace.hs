-- | Utilities for traversing heterogeneous trees. A heterogeneous tree is a
-- collection of datatypes that "contain" eachother in some form of tree
-- structure.
module OpenAPI.Checker.Trace
  ( Steppable (..)
  , Trace (..)
  , catTrace
  , DiffTrace (..)
  , catDiffTrace
  , _DiffTrace
  , AnItem (..)
  , step
  , Traced (..)
  , mapTraced
  , retrace
  , deTraced

    -- * Reexports
  , (>>>)
  , (<<<)
  )
where

import Control.Category
import Control.Lens
import Data.Kind
import Data.Type.Equality
import Type.Reflection
import Prelude hiding ((.))

class
  (Typeable a, Typeable b, Ord (Step a b)) =>
  Steppable (a :: k) (b :: k)
  where
  -- | How to get from an @a@ node to a @b@ node
  data Step (a :: k) (b :: k) :: Type

-- | How to get from an @a@ node to a @b@ node in possibly multiple steps. Like
-- a list, but indexed. The list is in reverse because in practice we append
-- items at the end one at a time.
data Trace (a :: k) (b :: k) where
  Root :: Trace a a
  Snoc :: Steppable b c => Trace a b -> !(Step b c) -> Trace a c

infixl 5 `Snoc`

step :: Steppable a b => Step a b -> Trace a b
step s = Root `Snoc` s

instance Category Trace where
  id = Root
  (.) = flip catTrace

typeRepRHS :: Typeable b => Trace a b -> TypeRep b
typeRepRHS _ = typeRep

typeRepLHS :: Typeable b => Trace a b -> TypeRep a
typeRepLHS Root = typeRep
typeRepLHS (Snoc xs _) = typeRepLHS xs

instance TestEquality (Trace a) where
  testEquality Root Root = Just Refl
  testEquality Root (Snoc ys _) = testEquality (typeRepLHS ys) typeRep
  testEquality (Snoc xs _) Root = testEquality typeRep (typeRepLHS xs)
  testEquality (Snoc _ _) (Snoc _ _) = testEquality typeRep typeRep

instance Eq (Trace a b) where
  Root == Root = True
  Snoc xs x == Snoc ys y
    | Just Refl <- testEquality (typeRepRHS xs) (typeRepRHS ys) =
      xs == ys && x == y
  _ == _ = False

instance Ord (Trace a b) where
  compare Root Root = EQ
  compare Root (Snoc _ _) = LT
  compare (Snoc _ _) Root = GT
  compare (Snoc xs x) (Snoc ys y) =
    case testEquality (typeRepRHS xs) (typeRepRHS ys) of
      Just Refl -> compare xs ys <> compare x y
      Nothing -> compare (someTypeRep xs) (someTypeRep ys)

catTrace :: Trace a b -> Trace b c -> Trace a c
catTrace xs Root = xs
catTrace xs (Snoc ys y) = Snoc (catTrace xs ys) y

infixl 5 `catTrace`

-- | Like a differece list, but indexed.
newtype DiffTrace (a :: k) (b :: k)
  = DiffTrace (forall c. Trace c a -> Trace c b)

catDiffTrace :: DiffTrace a b -> DiffTrace b c -> DiffTrace a c
catDiffTrace (DiffTrace f) (DiffTrace g) = DiffTrace (g . f)

_DiffTrace :: Iso (DiffTrace a b) (DiffTrace c d) (Trace a b) (Trace c d)
_DiffTrace = dimap (\(DiffTrace f) -> f Root) $
  fmap $ \xs -> DiffTrace (`catTrace` xs)

-- | An item related to some path relative to the root @r@.
data AnItem (f :: k -> Type) (r :: k) where
  AnItem :: Ord (f a) => Trace r a -> !(f a) -> AnItem f r

-- the Ord is yuck but we need it and it should be fine in monomorphic cases

instance Eq (AnItem f r) where
  AnItem xs fx == AnItem ys fy
    | Just Refl <- testEquality xs ys =
      xs == ys && fx == fy
  _ == _ = False

instance Typeable r => Ord (AnItem f r) where
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

data Traced root a = Traced {getTrace :: Trace root a, getTraced :: a}

mapTraced :: (Trace root a -> Trace root b) -> (a -> b) -> Traced root a -> Traced root b
mapTraced f g (Traced t a) = Traced (f t) (g a)

retrace :: (Trace root a -> Trace raat a) -> Traced root a -> Traced raat a
retrace f (Traced t a) = Traced (f t) a

deTraced :: Traced root a -> (Trace root a, a)
deTraced (Traced a b) = (a, b)

-- type APath = AnItem Proxy
