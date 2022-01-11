module Data.OpenApi.Compare.Formula
  ( FormulaF,
    VarRef,
    variable,
    eitherOf,
    anError,
    errors,
    calculate,
    maxFixpoint,
    mapErrors,
  )
where

import Data.Kind
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.OpenApi.Compare.Paths
import qualified Data.OpenApi.Compare.PathsPrefixTree as P

type VarRef = Int

-- | The type @FormulaF f r ()@ describes (modulo contents of errors) boolean
-- formulas involving variables, conjunctions, and disjunctions. These
-- operations (and the generated algebra) are monotonous. This ensures that
-- fixpoints always exist, i.e. that @x = f x@ has at least one solution.
data FormulaF (q :: k -> k -> Type) (f :: k -> Type) (r :: k) (a :: Type) where
  Result :: a -> FormulaF q f r a
  Errors ::
    !(P.PathsPrefixTree q f r) ->
    -- | invariant: never empty
    FormulaF q f r a
  Apply ::
    FormulaF q f r (b -> c) ->
    FormulaF q f r b ->
    (c -> a) ->
    -- | invariant: at least one of LHS and RHS is not 'Errors', and they are
    -- both not 'Result'
    FormulaF q f r a
  SelectFirst ::
    NE.NonEmpty (SomeFormulaF b) ->
    !(P.PathsPrefixTree q f r) ->
    (b -> a) ->
    -- | invariant: the list doesn't contain any 'Result's, 'Errors' or
    -- 'SelectFirst'
    FormulaF q f r a
  Variable :: !VarRef -> a -> FormulaF q f r a

mkApply :: FormulaF q f r (b -> c) -> FormulaF q f r b -> (c -> a) -> FormulaF q f r a
mkApply (Result f) x h = h . f <$> x
mkApply f (Result x) h = h . ($ x) <$> f
mkApply (Errors e1) (Errors e2) _ = Errors (e1 <> e2)
mkApply f x h = Apply f x h

mkSelectFirst :: [SomeFormulaF b] -> P.PathsPrefixTree q f r -> (b -> a) -> FormulaF q f r a
mkSelectFirst fs allE h = case foldMap check fs of
  (First (Just x), _) -> Result (h x)
  (First Nothing, x : xs) -> SelectFirst (x NE.:| xs) allE h
  (First Nothing, []) -> Errors allE
  where
    check (SomeFormulaF (Result x)) = (First (Just x), mempty)
    check (SomeFormulaF (Errors _)) = (mempty, mempty)
    check (SomeFormulaF (SelectFirst xs _ h')) =
      (mempty, NE.toList (fmap (fmap h') xs))
    check x = (mempty, [x])

data SomeFormulaF (a :: Type) where
  SomeFormulaF :: FormulaF q f r a -> SomeFormulaF a

anError :: AnItem q f r -> FormulaF q f r a
anError e = Errors $ P.singleton e

errors :: P.PathsPrefixTree q f r -> FormulaF q f r ()
errors t
  | P.null t = Result ()
  | otherwise = Errors t

variable :: VarRef -> FormulaF q f r ()
variable v = Variable v ()

instance Functor (FormulaF q f r) where
  fmap f (Result x) = Result (f x)
  fmap _ (Errors e) = Errors e
  fmap f (Apply g x h) = Apply g x (f . h)
  fmap f (SelectFirst xs e h) = SelectFirst xs e (f . h)
  fmap f (Variable r x) = Variable r (f x)

instance Functor SomeFormulaF where
  fmap f (SomeFormulaF x) = SomeFormulaF (fmap f x)

instance Applicative (FormulaF q f r) where
  pure = Result
  f <*> x = mkApply f x id

eitherOf :: [FormulaF q' f' r' a] -> AnItem q f r -> FormulaF q f r a
eitherOf fs allE = mkSelectFirst (map SomeFormulaF fs) (P.singleton allE) id

calculate :: FormulaF q f r a -> Either (P.PathsPrefixTree q f r) a
calculate (Result x) = Right x
calculate (Errors e) = Left e
calculate (Apply f x h) = case calculate f of
  Left e1 -> case calculate x of
    Left e2 -> Left (e1 <> e2)
    Right _ -> Left e1
  Right f' -> case calculate x of
    Left e2 -> Left e2
    Right x' -> Right (h (f' x'))
calculate (SelectFirst xs e h) = go (NE.toList xs)
  where
    go (SomeFormulaF r : rs) = case calculate r of
      Left _ -> go rs
      Right x -> Right (h x)
    go [] = Left e
calculate (Variable i _) = error $ "Unknown variable " <> show i

-- Approximate for now. Answers yes/no correctly, but the error lists aren't
-- super accurate. TODO: improve
maxFixpoint :: VarRef -> FormulaF q f r () -> FormulaF q f r ()
maxFixpoint i = go
  where
    go :: FormulaF q f r a -> FormulaF q f r a
    go (Result x) = Result x
    go (Errors e) = Errors e
    go (Apply f x h) = mkApply (go f) (go x) h
    go (SelectFirst fs e h) = mkSelectFirst (NE.toList (fmap goSF fs)) e h
    go (Variable j x) | i == j = Result x
    go v@(Variable _ _) = v
    goSF :: SomeFormulaF a -> SomeFormulaF a
    goSF (SomeFormulaF x) = SomeFormulaF (go x)

mapErrors :: (P.PathsPrefixTree q f r -> P.PathsPrefixTree q' f' r') -> FormulaF q f r a -> FormulaF q' f' r' a
mapErrors _ (Result x) = Result x
mapErrors m (Errors e) = Errors $ m e
mapErrors m (Apply f x h) = mkApply (mapErrors m f) (mapErrors m x) h
mapErrors m (SelectFirst fs e h) = mkSelectFirst (NE.toList fs) (m e) h
mapErrors _ (Variable i x) = Variable i x
