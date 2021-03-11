{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Utilities for effectfully memoizing other, more effectful functions.
module OpenAPI.Checker.Memo
  ( Progress(..)
  , MonadMemo
  , MemoState
  , runMemo
  , tryMemo
  , tryMemoWithTag
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Tagged
import qualified Data.TypeRepMap as T
import Type.Reflection

-- | A computation has either 'Finished', or it is 'InProgress'.
data Progress a = Finished a | InProgress
  deriving (Eq, Ord, Show)

data MemoMap a where
  MemoMap :: !(M.Map k (Progress v)) -> MemoMap (k, v)

newtype MemoState = MemoState (T.TypeRepMap MemoMap)

-- | An effectful memoization monad.
type MonadMemo m = MonadState MemoState m

memoStateLookup
  :: forall k v. (Typeable k, Typeable v, Ord k)
  => k -> MemoState -> Maybe (Progress v)
memoStateLookup k (MemoState t) = case T.lookup @(k, v) t of
  Just (MemoMap m) -> M.lookup k m
  Nothing -> Nothing

memoStateInsert
  :: forall k v. (Typeable k, Typeable v, Ord k)
  => k -> Progress v -> MemoState -> MemoState
memoStateInsert k x (MemoState t) = MemoState $ T.insert (MemoMap m'') t
  where
    m'' = M.insert k x m'
    m' = case T.lookup @(k, v) t of
      Just (MemoMap m) -> m
      Nothing -> M.empty

-- | Run a memoized computation.
runMemo :: Monad m => StateT MemoState m a -> m a
runMemo = (`evalStateT` MemoState T.empty)

-- | Try to call a function whilst memoizing its result. If we are already
-- inside an evaluation of some function with these arguments, this returns
-- @InProgress@. Beware of different functions that have the same type.
tryMemo
  :: forall k v m. (Typeable k, Typeable v, Ord k, MonadMemo m)
  => (k -> m v) -> k -> m (Progress v)
tryMemo f k = memoStateLookup k <$> get >>= \case
  Just x -> pure x
  Nothing -> do
    modify $ memoStateInsert k (InProgress @v)
    result <- Finished <$> f k
    modify $ memoStateInsert k result
    pure result

-- | Disambiguate memoized computations with an arbitrary tag.
tryMemoWithTag
  :: forall t k v m. (Typeable t, Typeable k, Typeable v, Ord k, MonadMemo m)
  => (k -> m v) -> k -> m (Progress v)
tryMemoWithTag f k = withTypeable (typeRepKind $ typeRep @t) $
  tryMemo (f . unTagged) (Tagged @t k)
