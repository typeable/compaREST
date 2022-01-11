{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Utilities for effectfully memoizing other, more effectful functions.
module Data.OpenApi.Compare.Memo
  ( MonadMemo,
    MemoState,
    runMemo,
    modifyMemoNonce,
    KnotTier (..),
    unknot,
    memoWithKnot,
    memoTaggedWithKnot,
  )
where

import Control.Monad.State
import Data.Dynamic
import qualified Data.Map as M
import Data.Tagged
import qualified Data.TypeRepMap as T
import Data.Void
import Type.Reflection

data Progress a = Finished a | Started | TyingKnot Dynamic

data MemoMap a where
  MemoMap :: !(M.Map k (Progress v)) -> MemoMap (k, v)

data MemoState s = MemoState s (T.TypeRepMap MemoMap)

-- | An effectful memoization monad.
type MonadMemo s m = MonadState (MemoState s) m

memoStateLookup ::
  forall k v s.
  (Typeable k, Typeable v, Ord k) =>
  k ->
  MemoState s ->
  Maybe (Progress v)
memoStateLookup k (MemoState _ t) = case T.lookup @(k, v) t of
  Just (MemoMap m) -> M.lookup k m
  Nothing -> Nothing

memoStateInsert ::
  forall k v s.
  (Typeable k, Typeable v, Ord k) =>
  k ->
  Progress v ->
  MemoState s ->
  MemoState s
memoStateInsert k x (MemoState s t) = MemoState s $ T.insert (MemoMap m'') t
  where
    m'' = M.insert k x m'
    m' = case T.lookup @(k, v) t of
      Just (MemoMap m) -> m
      Nothing -> M.empty

modifyMemoNonce :: MonadMemo s m => (s -> s) -> m s
modifyMemoNonce f = do
  MemoState s t <- get
  put $ MemoState (f s) t
  pure s

-- | Run a memoized computation.
runMemo :: Monad m => s -> StateT (MemoState s) m a -> m a
runMemo s = (`evalStateT` MemoState s T.empty)

-- | A description of how to effectfully tie knots in type @v@, using the @m@
-- monad, and by sharing some @d@ data among the recursive instances.
data KnotTier v d m = KnotTier
  { -- | Create some data that will be connected to this knot
    onKnotFound :: m d
  , -- | This is what the knot will look like as a value
    -- to the inner computations
    onKnotUsed :: d -> m v
  , -- | Once we're done and we're outside, tie the
    -- knot using the datum
    tieKnot :: d -> v -> m v
  }

unknot :: KnotTier v Void m
unknot =
  KnotTier
    { onKnotFound = error "Recursion detected"
    , onKnotUsed = absurd
    , tieKnot = absurd
    }

-- | Run a potentially recursive computation. The provided key will be used to
-- refer to the result of this computation. If during the computation, another
-- attempt to run the computation with the same key is made, we run a
-- tying-the-knot procedure.
--
-- If another attempt to run the computation with the same key is made
-- *after we're done*, we will return the memoized value.
memoWithKnot ::
  forall k v d m s.
  (Typeable k, Typeable v, Typeable d, Ord k, MonadMemo s m) =>
  KnotTier v d m ->
  -- | the computation to memoize
  m v ->
  -- | key for memoization
  k ->
  m v
memoWithKnot tier f k =
  memoStateLookup @k @v k <$> get >>= \case
    Just (Finished v) -> pure v
    Just Started -> do
      d <- onKnotFound tier
      modify $ memoStateInsert @k @v k (TyingKnot $ toDyn d)
      onKnotUsed tier d
    Just (TyingKnot dyn) -> case fromDynamic dyn of
      Just d -> onKnotUsed tier d
      Nothing ->
        error $
          "Type mismatch when examining the knot of "
            <> show (typeRep @(k -> v))
            <> ": expected "
            <> show (typeRep @d)
            <> ", got "
            <> show (dynTypeRep dyn)
    Nothing -> do
      modify $ memoStateInsert @k @v k Started
      v <- f
      v' <-
        memoStateLookup @k @v k <$> get >>= \case
          Just Started -> pure v
          Just (TyingKnot dyn) -> case fromDynamic dyn of
            Just d -> tieKnot tier d v
            Nothing ->
              error $
                "Type mismatch when tying the knot of "
                  <> show (typeRep @(k -> v))
                  <> ": expected "
                  <> show (typeRep @d)
                  <> ", got "
                  <> show (dynTypeRep dyn)
          Just (Finished _) ->
            error $
              "Unexpected Finished when memoizing "
                <> show (typeRep @(k -> v))
          Nothing -> pure v
      -- Normally this would be an error, but the underlying monad can refuse
      -- to remember memoization state
      modify $ memoStateInsert @k @v k (Finished v')
      pure v'

-- | Disambiguate memoized computations with an arbitrary tag.
memoTaggedWithKnot ::
  forall t k v d m s.
  ( Typeable t
  , Typeable k
  , Typeable v
  , Typeable d
  , Ord k
  , MonadMemo s m
  ) =>
  KnotTier v d m ->
  m v ->
  k ->
  m v
memoTaggedWithKnot tier f k =
  withTypeable (typeRepKind $ typeRep @t) $
    memoWithKnot tier f (Tagged @t k)
