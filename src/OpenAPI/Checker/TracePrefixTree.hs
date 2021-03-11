{-# LANGUAGE QuantifiedConstraints #-}
module OpenAPI.Checker.TracePrefixTree
  ( TracePrefixTree
  , empty
  , singleton
  , fromList
  , null
  , foldWith
  , toList
  , filter
  ) where

import qualified GHC.Exts as T
import Data.Foldable hiding (null, toList)
import Data.Kind
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.TypeRepMap as T
import OpenAPI.Checker.Trace
import Prelude hiding (null, filter)
import Type.Reflection
import Data.Type.Equality

-- | A list of @AnItem r f@, but optimized into a prefix tree.
data TracePrefixTree (f :: k -> Type) (r :: k) = TracePrefixTree
    { rootItems :: !(ASet (f r))
    , snocItems :: !(T.TypeRepMap (AStep f r))
    }

deriving instance Eq (TracePrefixTree f a)

-- Kind of orphan. Treat the map as an infinite tuple of @Maybe (f a)@'s, where
-- the components are ordered by the @SomeTypeRep@ of the @a@.
compareTRM
  :: (forall a. Typeable a => Ord (f a))
  => T.TypeRepMap f -> T.TypeRepMap f -> Ordering
compareTRM s1 s2
  = foldMap (\k -> compareMaybe compareW (M.lookup k m1) (M.lookup k m2)) mKeys
  where
    (m1, m2) = (toMap s1, toMap s2)
    mKeys = S.toAscList $ M.keysSet m1 `S.union` M.keysSet m2
    compareMaybe _   Nothing  Nothing  = EQ
    compareMaybe _   Nothing  (Just _) = LT
    compareMaybe _   (Just _) Nothing  = GT
    compareMaybe cmp (Just x) (Just y) = cmp x y
    compareW
      :: (forall a. Typeable a => Ord (f a))
      => T.WrapTypeable f -> T.WrapTypeable f -> Ordering
    compareW (T.WrapTypeable (x :: f a)) (T.WrapTypeable (y :: f b))
      | Just Refl <- testEquality (typeRep @a) (typeRep @b) = compare x y
      | otherwise = EQ -- unreachable
    toMap s = M.fromList
      [(someTypeRep x, w) | w@(T.WrapTypeable x) <- T.toList s]

instance Ord (TracePrefixTree f a) where
  compare (TracePrefixTree r1 s1) (TracePrefixTree r2 s2)
    = compare r1 r2 <> compareTRM s1 s2

data ASet (a :: Type) where
  AnEmptySet :: ASet a
  ASet :: Ord a => S.Set a -> ASet a

instance Semigroup (ASet a) where
  AnEmptySet <> s = s
  s <> AnEmptySet = s
  ASet s1 <> ASet s2 = ASet $ S.union s1 s2

deriving instance Eq (ASet a)
deriving instance Ord (ASet a)

-- type traceprefixset = traceprefixtree proxy

instance Monoid (ASet a) where
  mempty = AnEmptySet

data AStep (f :: k -> Type) (r :: k) (a :: k) where
  AStep
    :: Steppable r a => !(M.Map (Step r a) (TracePrefixTree f a)) -> AStep f r a

deriving instance Eq (AStep f r a)
deriving instance Ord (AStep f r a)

singleton :: AnItem f r -> TracePrefixTree f r
singleton (AnItem ys v) = go ys $ TracePrefixTree (ASet $ S.singleton v) T.empty
  where
    go :: Trace r a -> TracePrefixTree f a -> TracePrefixTree f r
    go Root !t = t
    go (Snoc xs x) !t = go xs $ TracePrefixTree AnEmptySet $ T.one
      $ AStep $ M.singleton x t

instance Semigroup (TracePrefixTree f r) where
  TracePrefixTree r1 s1 <> TracePrefixTree r2 s2 =
    TracePrefixTree (r1 <> r2) (T.unionWith joinSteps s1 s2)
    where joinSteps (AStep m1) (AStep m2) = AStep $ M.unionWith (<>) m1 m2

instance Monoid (TracePrefixTree f r) where
  mempty = TracePrefixTree mempty T.empty

empty :: TracePrefixTree f r
empty = mempty

fromList :: [AnItem f r] -> TracePrefixTree f r
fromList = foldMap singleton

null :: TracePrefixTree f r -> Bool
null (TracePrefixTree AnEmptySet s) = T.size s == 0
null _ = False

foldWith
  :: forall f m r. Monoid m
  => (forall a. Ord (f a) => Trace r a -> f a -> m) -> TracePrefixTree f r -> m
foldWith k = goTPT Root
  where
    goTPT :: forall a. Trace r a -> TracePrefixTree f a -> m
    goTPT xs t = goASet xs (rootItems t) <> goTRM xs (snocItems t)
    goASet :: forall a. Trace r a -> ASet (f a) -> m
    goASet _ AnEmptySet = mempty
    goASet xs (ASet rs) = foldMap (k xs) rs
    goTRM :: forall a. Trace r a -> T.TypeRepMap (AStep f a) -> m
    goTRM xs s = foldMap (\(T.WrapTypeable f) -> goAStep xs f) $ T.toList s
    goAStep :: forall a b. Trace r a -> AStep f a b -> m
    goAStep xs (AStep m)
      = M.foldrWithKey (\x t -> (goTPT (Snoc xs x) t <>)) mempty m

toList :: TracePrefixTree f r -> [AnItem f r]
toList t = appEndo (foldWith (\xs f -> Endo (AnItem xs f:)) t) []

-- | Select a subtree by prefix
filter :: Trace r a -> TracePrefixTree f r -> TracePrefixTree f a
filter Root t = t
filter (Snoc xs x) t = foldMap (\(AStep m) -> fold $ M.lookup x m)
  $ T.lookup $ snocItems $ filter xs t
