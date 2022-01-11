module Data.OpenUnion.Extra
  ( (@@>),
    TryLiftUnion (..),
    pattern SingletonUnion,
  )
where

import Control.Applicative
import Data.Dynamic
import Data.OpenUnion.Internal
import Data.Typeable
import TypeFun.Data.List hiding (Union)

class TryLiftUnion xs where
  tryLiftUnion :: (Alternative m, Typeable x) => x -> m (Union xs)

instance TryLiftUnion '[] where
  tryLiftUnion _ = empty

instance
  (Typeable y, SubList ys (y : ys), TryLiftUnion ys) =>
  TryLiftUnion (y ': ys)
  where
  tryLiftUnion (x :: x) = case eqT @x @y of
    Nothing -> reUnion <$> tryLiftUnion @ys x
    Just Refl -> pure $ liftUnion x

-- | Like '@>', but enforces a specific type list order.
-- (Useful for deconstruction-directed type inference.)
(@@>) :: Typeable a => (a -> b) -> (Union xs -> b) -> Union (a ': xs) -> b
r @@> l = either l r . restrict'
  where
    restrict' :: Typeable a => Union (a ': aa) -> Either (Union aa) a
    restrict' (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE (@@>) #-}

infixr 2 @@>

pattern SingletonUnion :: (Typeable a, Elem a s) => a -> Union s
pattern SingletonUnion x <-
  ((\(Union y) -> fromDynamic y) -> Just x)
  where
    SingletonUnion x = liftUnion x
