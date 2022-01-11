module Data.OpenApi.Compare.Validate.Schema.DNF
  ( DNF (..),
    Disjunct (..),
    pattern SingleDisjunct,
    pattern TopDNF,
    pattern BottomDNF,
    pattern LiteralDNF,
    foldDNF,
    forDNF,
  )
where

import Algebra.Lattice
import Control.Applicative
import Data.Foldable
import qualified Data.Set as S

-- | A boolean formula (without "not") represented as a Disjunctive Normal Form:
-- the formula is a disjunction of a set of clauses, each of which is a
-- conjunction of a set of some elementary formulas.
-- Invariant: no two disjuncts imply eachother
newtype DNF a = DNF (S.Set (Disjunct a))
  deriving stock (Eq, Ord, Show)

-- A disjunct is a thing that is to be disjuncted. Itself it is a conjunction of some literals.
newtype Disjunct a = Disjunct (S.Set a)
  deriving stock (Eq, Ord, Show)

disjImplies :: Ord a => Disjunct a -> Disjunct a -> Bool
disjImplies (Disjunct xs) (Disjunct ys) = ys `S.isSubsetOf` xs

disjConjunction :: Ord a => Disjunct a -> Disjunct a -> Disjunct a
disjConjunction (Disjunct xs) (Disjunct ys) = Disjunct $ xs `S.union` ys

disjAdd :: Ord a => DNF a -> Disjunct a -> DNF a
disjAdd (DNF yss) xs
  | any (xs `disjImplies`) yss = DNF yss
  | otherwise = DNF $ S.insert xs $ S.filter (not . (`disjImplies` xs)) yss

instance Ord a => Lattice (DNF a) where
  xss \/ DNF yss = S.foldl' disjAdd xss yss
  DNF xss /\ DNF yss =
    foldl' disjAdd bottom $
      liftA2 disjConjunction (S.toList xss) (S.toList yss)

pattern BottomDNF :: DNF a
pattern BottomDNF <-
  DNF (S.null -> True)
  where
    BottomDNF = DNF S.empty

isSingleton :: S.Set a -> Maybe a
isSingleton s
  | S.size s == 1 = S.lookupMin s
  | otherwise = Nothing

pattern SingleDisjunct :: Ord a => Disjunct a -> DNF a
pattern SingleDisjunct xs <-
  DNF (isSingleton -> Just xs)
  where
    SingleDisjunct xs = DNF $ S.singleton xs

pattern TopDNF :: DNF a
pattern TopDNF <-
  DNF (isSingleton -> Just (Disjunct (S.null -> True)))
  where
    TopDNF = DNF $ S.singleton $ Disjunct S.empty

pattern LiteralDNF :: Ord a => a -> DNF a
pattern LiteralDNF x <-
  SingleDisjunct (Disjunct (isSingleton -> Just x))
  where
    LiteralDNF x = SingleDisjunct $ Disjunct $ S.singleton x

instance Ord a => BoundedJoinSemiLattice (DNF a) where
  bottom = BottomDNF

instance Ord a => BoundedMeetSemiLattice (DNF a) where
  top = TopDNF

foldDisjunct :: BoundedMeetSemiLattice l => (a -> l) -> Disjunct a -> l
foldDisjunct f (Disjunct xs) = S.foldl' (\y l -> y /\ f l) top xs

foldDNF :: BoundedLattice l => (a -> l) -> DNF a -> l
foldDNF f (DNF xss) = S.foldl' (\y xs -> y \/ foldDisjunct f xs) bottom xss

newtype LiftA f a = LiftA {getLiftA :: f a}
  deriving newtype (Functor, Applicative)

instance (Lattice a, Applicative f) => Lattice (LiftA f a) where
  (/\) = liftA2 (/\)
  (\/) = liftA2 (\/)

instance (BoundedJoinSemiLattice a, Applicative f) => BoundedJoinSemiLattice (LiftA f a) where
  bottom = pure bottom

instance (BoundedMeetSemiLattice a, Applicative f) => BoundedMeetSemiLattice (LiftA f a) where
  top = pure top

forDNF :: (BoundedLattice l, Applicative f) => (a -> f l) -> DNF a -> f l
forDNF f = getLiftA . foldDNF (LiftA . f)
