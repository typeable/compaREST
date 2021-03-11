module OpenAPI.Checker.Formula
  ( FormulaF
  , VarRef
  , anyOf
  , anError
  , calculate
  ) where

import Data.Kind
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

type VarRef = Int

-- | The type @FormulaF f r ()@ describes (modulo contents of errors) boolean
-- formulas involving variables, conjunctions, and disjunctions. These
-- operations (and the generated algebra) are monotonous. This ensures that
-- there always exist fixpoints, i.e. that @x = f x@ has at least one solution.
data FormulaF (f :: k -> Type) (r :: k) (a :: Type) where
  Result :: a -> FormulaF f r a
  Errors :: !(T.TracePrefixTree f r) -> FormulaF f r a
    -- ^ invariant: never empty
  Apply :: FormulaF f r (b -> c) -> FormulaF f r b -> (c -> a) -> FormulaF f r a
    -- ^ invariant: LHS and RHS are never 'Result' nor 'Errors'
  SelectFirst :: NE.NonEmpty (FormulaF f r b)
    -> !(AnItem f r) -> (b -> a) -> FormulaF f r a
    -- ^ invariant: the list doesn't contain any 'Result's or 'Errors'.
  Variable :: !VarRef -> FormulaF f r a

anError :: AnItem f r -> FormulaF f r a
anError e = Errors $ T.singleton e

instance Functor (FormulaF f r) where
  fmap f (Result x) = Result (f x)
  fmap _ (Errors e) = Errors e
  fmap f (Apply g x h) = Apply g x (f . h)
  fmap f (SelectFirst xs e h) = SelectFirst xs e (f . h)
  fmap _ (Variable r) = Variable r

instance Applicative (FormulaF f r) where
  pure = Result
  Result f <*> x = f <$> x
  Errors e <*> Result _ = Errors e
  Errors e1 <*> Errors e2 = Errors (e1 <> e2)
  f <*> x = Apply f x id

anyOf :: [FormulaF f r a] -> AnItem f r -> FormulaF f r a
anyOf fs allE = case foldMap check fs of
  (First (Just x), _) -> Result x
  (First Nothing, (x:xs)) -> SelectFirst (x NE.:| xs) allE id
  (First Nothing, []) -> Errors $ T.singleton allE
  where
    check (Result x) = (First (Just x), mempty)
    check (Errors _) = (mempty, mempty)
    check (SelectFirst xs _ h) = (mempty, NE.toList (fmap (fmap h) xs))
    check x = (mempty, [x])

calculate :: FormulaF f r a -> Either (T.TracePrefixTree f r) a
calculate (Result x) = Right x
calculate (Errors e) = Left e
calculate (Apply f x h) = case calculate f of
  Left e1 -> case calculate x of
    Left e2 -> Left (e1 <> e2)
    Right _ -> Left e1
  Right f' -> case calculate x of
    Left e2 -> Left e2
    Right x' -> Right (h (f' x'))
calculate (SelectFirst xs e h) = go (calculate <$> NE.toList xs)
  where
    go (Left _ : rs) = go rs
    go (Right x : _) = Right (h x)
    go [] = Left $ T.singleton e
calculate (Variable i) = error $ "Unknown variable " <> show i
