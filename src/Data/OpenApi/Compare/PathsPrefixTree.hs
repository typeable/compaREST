{-# LANGUAGE QuantifiedConstraints #-}

module Data.OpenApi.Compare.PathsPrefixTree
  ( PathsPrefixTree (PathsPrefixNode),
    AStep (..),
    empty,
    singleton,
    fromList,
    null,
    foldWith,
    toList,
    filter,
    filterWithKey,
    takeSubtree,
    lookup,
    embed,
    size,
    partition,
    map,
  )
where

import Control.Monad
import Data.Aeson
import Data.Foldable hiding (null, toList)
import qualified Data.HashMap.Strict as HM
import Data.Kind
import qualified Data.Map as M
import Data.Monoid
import Data.OpenApi.Compare.Paths
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Type.Equality
import qualified Data.TypeRepMap as TRM
import qualified Data.Vector as V
import qualified GHC.Exts as Exts
import Type.Reflection
import Prelude hiding (filter, lookup, map, null)

-- | A list of @AnItem r f@, but optimized into a prefix tree.
data PathsPrefixTree (q :: k -> k -> Type) (f :: k -> Type) (r :: k) = PathsPrefixTree
  { rootItems :: !(ASet (f r))
  , snocItems :: !(TRM.TypeRepMap (AStep q f r))
  }
  deriving stock (Show)

map :: (forall x. f x -> f x) -> PathsPrefixTree q f r -> PathsPrefixTree q f r
map f (PathsPrefixTree roots branches) =
  PathsPrefixTree (mapASet f roots) (TRM.hoist (mapAStep f) branches)

-- TODO: optimize
partition :: (forall a. f a -> Bool) -> PathsPrefixTree q f r -> (PathsPrefixTree q f r, PathsPrefixTree q f r)
partition f x = (filter f x, filter (not . f) x)

filter :: (forall a. f a -> Bool) -> PathsPrefixTree q f r -> PathsPrefixTree q f r
filter f (PathsPrefixTree roots branches) = PathsPrefixTree roots' branches'
  where
    roots' = filterASet f roots
    branches' =
      Exts.fromList
        . fmap (\(TRM.WrapTypeable (AStep x)) -> TRM.WrapTypeable . AStep . M.mapMaybe (maybeNonEmpty . filter f) $ x)
        . Exts.toList
        $ branches

    maybeNonEmpty = mfilter (not . null) . Just

filterWithKey :: (forall a. Paths q r a -> f a -> Bool) -> PathsPrefixTree q f r -> PathsPrefixTree q f r
filterWithKey = go Root
  where
    go :: Paths q r b -> (forall a. Paths q r a -> f a -> Bool) -> PathsPrefixTree q f b -> PathsPrefixTree q f b
    go xs f (PathsPrefixTree roots branches) = PathsPrefixTree roots' branches'
      where
        roots' = filterASet (f xs) roots
        branches' =
          Exts.fromList
            . fmap (\(TRM.WrapTypeable (AStep x)) -> TRM.WrapTypeable . AStep . M.mapMaybeWithKey (\k -> maybeNonEmpty . go (xs `Snoc` k) f) $ x)
            . Exts.toList
            $ branches

    maybeNonEmpty = mfilter (not . null) . Just

-- | The number of leaves.
size :: PathsPrefixTree q f r -> Int
size (PathsPrefixTree root branches) =
  (S.size . toSet $ root)
    + (sum . fmap (\(TRM.WrapTypeable (AStep x)) -> sum . fmap size . M.elems $ x) . Exts.toList $ branches)

pattern PathsPrefixNode :: Ord (f r) => S.Set (f r) -> [TRM.WrapTypeable (AStep q f r)] -> PathsPrefixTree q f r
pattern PathsPrefixNode s steps <-
  (\(PathsPrefixTree aset m) -> (toSet aset, Exts.toList m) -> (s, steps))
  where
    PathsPrefixNode s steps = PathsPrefixTree (fromSet s) (Exts.fromList steps)

{-# COMPLETE PathsPrefixNode #-}

instance (forall a. ToJSON (f a)) => ToJSON (PathsPrefixTree q f r) where
  toJSON =
    Object . getMergableObject
      . foldWith (\t x -> MergableObject . traceObject t $ toJSON x)

deriving stock instance Eq (PathsPrefixTree q f a)

-- Kind of orphan. Treat the map as an infinite tuple of @Maybe (f a)@'s, where
-- the components are ordered by the @SomeTypeRep@ of the @a@.
compareTRM ::
  (forall a. Typeable a => Ord (f a)) =>
  TRM.TypeRepMap f ->
  TRM.TypeRepMap f ->
  Ordering
compareTRM s1 s2 =
  foldMap (\k -> compareMaybe compareW (M.lookup k m1) (M.lookup k m2)) mKeys
  where
    (m1, m2) = (toMap s1, toMap s2)
    mKeys = S.toAscList $ M.keysSet m1 `S.union` M.keysSet m2
    compareMaybe _ Nothing Nothing = EQ
    compareMaybe _ Nothing (Just _) = LT
    compareMaybe _ (Just _) Nothing = GT
    compareMaybe cmp (Just x) (Just y) = cmp x y
    compareW ::
      (forall a. Typeable a => Ord (f a)) =>
      TRM.WrapTypeable f ->
      TRM.WrapTypeable f ->
      Ordering
    compareW (TRM.WrapTypeable (x :: f a)) (TRM.WrapTypeable (y :: f b))
      | Just Refl <- testEquality (typeRep @a) (typeRep @b) = compare x y
      | otherwise = EQ -- unreachable
    toMap s =
      M.fromList
        [(someTypeRep x, w) | w@(TRM.WrapTypeable x) <- Exts.toList s]

instance Ord (PathsPrefixTree q f a) where
  compare (PathsPrefixTree r1 s1) (PathsPrefixTree r2 s2) =
    compare r1 r2 <> compareTRM s1 s2

filterASet :: (a -> Bool) -> ASet a -> ASet a
filterASet _ AnEmptySet = AnEmptySet
filterASet f (ASet s) = fromSet $ S.filter f s

data ASet (a :: Type) where
  AnEmptySet :: ASet a
  ASet :: Ord a => S.Set a -> ASet a

mapASet :: (Ord a => Ord b) => (a -> b) -> ASet a -> ASet b
mapASet _ AnEmptySet = AnEmptySet
mapASet f (ASet s) = ASet $ S.map f s

deriving stock instance Show a => Show (ASet a)

toSet :: ASet a -> S.Set a
toSet AnEmptySet = S.empty
toSet (ASet s) = s

fromSet :: Ord a => S.Set a -> ASet a
fromSet s | S.null s = AnEmptySet
fromSet s = ASet s

instance ToJSON a => ToJSON (ASet a) where
  toJSON = toJSON . toSet

instance Semigroup (ASet a) where
  AnEmptySet <> s = s
  s <> AnEmptySet = s
  ASet s1 <> ASet s2 = ASet $ S.union s1 s2

deriving stock instance Eq (ASet a)

deriving stock instance Ord (ASet a)

-- type traceprefixset = traceprefixtree proxy

instance Monoid (ASet a) where
  mempty = AnEmptySet

data AStep (q :: k -> k -> Type) (f :: k -> Type) (r :: k) (a :: k) where
  AStep ::
    NiceQuiver q r a =>
    !(M.Map (q r a) (PathsPrefixTree q f a)) ->
    AStep q f r a

mapAStep :: (forall x. f x -> f x) -> AStep q f r a -> AStep q f r a
mapAStep f (AStep m) = AStep $ M.map (map f) m

deriving stock instance Eq (AStep q f r a)

deriving stock instance Ord (AStep q f r a)

singleton :: AnItem q f r -> PathsPrefixTree q f r
singleton (AnItem ys v) = go ys $ PathsPrefixTree (ASet $ S.singleton v) TRM.empty
  where
    go :: Paths q r a -> PathsPrefixTree q f a -> PathsPrefixTree q f r
    go Root !t = t
    go (Snoc xs x) !t =
      go xs $
        PathsPrefixTree AnEmptySet $
          TRM.one $
            AStep $ M.singleton x t

instance Semigroup (PathsPrefixTree q f r) where
  PathsPrefixTree r1 s1 <> PathsPrefixTree r2 s2 =
    PathsPrefixTree (r1 <> r2) (TRM.unionWith joinSteps s1 s2)
    where
      joinSteps :: AStep q f r a -> AStep q f r a -> AStep q f r a
      joinSteps (AStep m1) (AStep m2) = AStep $ M.unionWith (<>) m1 m2

instance Monoid (PathsPrefixTree q f r) where
  mempty = PathsPrefixTree mempty TRM.empty

empty :: PathsPrefixTree q f r
empty = mempty

fromList :: [AnItem q f r] -> PathsPrefixTree q f r
fromList = foldMap singleton

null :: PathsPrefixTree q f r -> Bool
null (PathsPrefixTree AnEmptySet s) = all (\(TRM.WrapTypeable (AStep x)) -> all null x) (Exts.toList s)
null _ = False

foldWith ::
  forall q f m r.
  Monoid m =>
  (forall a. Ord (f a) => Paths q r a -> f a -> m) ->
  PathsPrefixTree q f r ->
  m
foldWith k = goTPT Root
  where
    goTPT :: forall a. Paths q r a -> PathsPrefixTree q f a -> m
    goTPT xs t = goASet xs (rootItems t) <> goTRM xs (snocItems t)
    goASet :: forall a. Paths q r a -> ASet (f a) -> m
    goASet _ AnEmptySet = mempty
    goASet xs (ASet rs) = foldMap (k xs) rs
    goTRM :: forall a. Paths q r a -> TRM.TypeRepMap (AStep q f a) -> m
    goTRM xs s = foldMap (\(TRM.WrapTypeable f) -> goAStep xs f) $ Exts.toList s
    goAStep :: forall a b. Paths q r a -> AStep q f a b -> m
    goAStep xs (AStep m) =
      M.foldrWithKey (\x t -> (goTPT (Snoc xs x) t <>)) mempty m

toList :: PathsPrefixTree q f r -> [AnItem q f r]
toList t = appEndo (foldWith (\xs f -> Endo (AnItem xs f :)) t) []

-- | Select a subtree by prefix
takeSubtree :: forall q f r a. Paths q r a -> PathsPrefixTree q f r -> PathsPrefixTree q f a
takeSubtree Root t = t
takeSubtree (Snoc xs x) t =
  foldMap (\(AStep m) -> fold $ M.lookup x m) $
    TRM.lookup @a $ snocItems $ takeSubtree xs t

lookup :: Paths q r a -> PathsPrefixTree q f r -> S.Set (f a)
lookup xs = toSet . rootItems . takeSubtree xs

-- | Embed a subtree in a larger tree with given prefix
embed :: Paths q r a -> PathsPrefixTree q f a -> PathsPrefixTree q f r
embed Root t = t
embed (Snoc xs x) t = embed xs $ PathsPrefixTree AnEmptySet $ TRM.one $ AStep $ M.singleton x t

newtype MergableObject = MergableObject {getMergableObject :: Object}

instance Semigroup MergableObject where
  (MergableObject x) <> (MergableObject y) =
    MergableObject $ HM.unionWith mergeValue x y
    where
      mergeValue :: Value -> Value -> Value
      mergeValue (Object a) (Object b) =
        Object . getMergableObject $ MergableObject a <> MergableObject b
      mergeValue (Array a) (Array b) = Array $ a <> b
      mergeValue (Array a) b = Array $ V.snoc a b
      mergeValue a (Array b) = Array $ V.cons a b
      mergeValue a b = toJSON [a, b]

instance Monoid MergableObject where
  mempty = MergableObject mempty

traceObject :: Paths q r a -> Value -> Object
traceObject Root (Object o) = o
traceObject Root v = HM.singleton "root" v
traceObject (root `Snoc` s) v =
  traceObject root . Object $ HM.singleton (T.pack . show $ s) v
