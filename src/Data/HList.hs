module Data.HList
  ( Has
  , HasAll
  , getH
  , HList (..)
  , singletonH
  )
where

import Data.Kind
import GHC.TypeLits

data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`

type family HasAll xs ys :: Constraint where
  HasAll '[] _ = ()
  HasAll (x ': xs) ys = (Has x ys, HasAll xs ys)

type Has x xs = Has' x xs (HeadEq x xs)

type family HeadEq x xs where
  HeadEq x (x ': _) = 'True
  HeadEq _ _ = 'False

class t ~ HeadEq x xs => Has' (x :: Type) (xs :: [Type]) (t :: Bool) where
  getH :: HList xs -> x

instance Has' x (x ': xs) 'True where
  getH (HCons x _) = x

instance (Has' x xs t, HeadEq x (y : xs) ~ 'False) => Has' x (y ': xs) 'False where
  getH (HCons _ xs) = getH xs

instance
  TypeError ('ShowType x ':<>: 'Text " is not a part of the list.")
  => Has' x '[] 'False
  where
  getH HNil = undefined

singletonH :: a -> HList '[a]
singletonH a = a `HCons` HNil

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (HCons x xs) == (HCons y ys) = x == y && xs == ys
