-- | Utilities for traversing heterogeneous trees. A heterogeneous tree is a
-- collection of datatypes that "contain" eachother in some form of tree
-- structure.

module OpenAPI.Checker.Trace
  ( Steppable(..)
  , Trace(..)
  , catTrace
  , DiffTrace(..)
  , catDiffTrace
  , _DiffTrace
  , AnItem(..)
  ) where

import Control.Lens
import Data.Kind
import Data.Typeable

class (Typeable a, Typeable b, Ord (Step a b))
  => Steppable (a :: k) (b :: k) where
  -- | How to get from an @a@ node to a @b@ node
  data family Step (a :: k) (b :: k) :: Type

-- | How to get from an @a@ node to a @b@ node in possibly multiple steps. Like
-- a list, but indexed. The list is in reverse because in practice we append
-- items at the end one at a time.
data Trace (a :: k) (b :: k) where
  Root :: Trace a a
  Snoc :: Steppable b c => Trace a b -> !(Step b c) -> Trace a c
infixl 5 `Snoc`

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
_DiffTrace = dimap (\(DiffTrace f) -> f Root)
  $ fmap $ \xs -> DiffTrace (`catTrace` xs)

-- | An item related to some path relative to the root @r@.
data AnItem (f :: k -> Type) (r :: k) where
  AnItem :: Ord (f a) => Trace r a -> !(f a) -> AnItem f r
  -- the Ord is yuck but we need it and it should be fine in monomorphic cases

-- type APath = AnItem Proxy
