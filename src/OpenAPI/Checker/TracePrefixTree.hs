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

-- | A list of @AnItem r f@, but optimized into a prefix tree.
data TracePrefixTree (f :: k -> Type) (r :: k) = TracePrefixTree
    { rootItems :: !(ASet (f r))
    , snocItems :: !(T.TypeRepMap (AStep f r))
    }

data ASet (a :: Type) where
  AnEmptySet :: ASet a
  ASet :: Ord a => S.Set a -> ASet a

instance Semigroup (ASet a) where
  AnEmptySet <> s = s
  s <> AnEmptySet = s
  ASet s1 <> ASet s2 = ASet $ S.union s1 s2

-- type traceprefixset = traceprefixtree proxy

instance Monoid (ASet a) where
  mempty = AnEmptySet

data AStep (f :: k -> Type) (r :: k) (a :: k) where
  AStep
    :: Steppable r a => !(M.Map (Step r a) (TracePrefixTree f a)) -> AStep f r a

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
