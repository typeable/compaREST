module OpenAPI.Checker.Subtree
  ( APIStep(..)
  , Subtree(..)
  , CompatM(..)
  , CompatFormula
  , runCompatFormula
  , localM
  , local'
  , issueAtTrace
  , issueAt
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind
import Data.OpenApi
import Data.Text
import Data.Functor.Compose
import OpenAPI.Checker.Formula
import OpenAPI.Checker.Memo
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

class (Subtree a, Subtree b, Steppable a b)
  => APIStep (a :: Type) (b :: Type) where
  describeStep :: Step a b -> Text

data ProdCons a = ProdCons
  { producer :: a
  , consumer :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative ProdCons where
  pure x = ProdCons x x
  ProdCons fp fc <*> ProdCons xp xc = ProdCons (fp xp) (fc xc)

data TracedEnv t = TracedEnv
  { getTrace :: Trace OpenApi t
  , getEnv :: CheckEnv t
  }

newtype CompatM t a = CompatM
  { unCompatM ::
    ReaderT (ProdCons (TracedEnv t))
      (StateT MemoState Identity) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (ProdCons (TracedEnv t))
    , MonadState MemoState
    )

type CompatFormula t = Compose (CompatM t) (FormulaF CheckIssue OpenApi)

class (Ord t, Ord (CheckIssue t)) => Subtree (t :: Type) where
  type family CheckEnv t :: Type
  data family CheckIssue t :: Type
  checkCompatibility :: ProdCons t -> CompatFormula t ()

runCompatFormula
  :: ProdCons (TracedEnv t)
  -> Compose (CompatM t) (FormulaF f r) a
  -> Either (T.TracePrefixTree f r) a
runCompatFormula env (Compose f)
  = calculate . runIdentity . runMemo . (`runReaderT` env) . unCompatM $ f

localM
  :: ProdCons (Trace a b)
  -> (ProdCons (CheckEnv a) -> ProdCons (CheckEnv b))
  -> CompatM b x
  -> CompatM a x
localM xs wrapEnv (CompatM k) = CompatM $ ReaderT $ \env ->
  runReaderT k $ TracedEnv
    <$> (catTrace <$> (getTrace <$> env) <*> xs)
    <*> wrapEnv (getEnv <$> env)

local'
  :: ProdCons (Trace a b)
  -> (ProdCons (CheckEnv a) -> ProdCons (CheckEnv b))
  -> Compose (CompatM b) (FormulaF f r) x
  -> Compose (CompatM a) (FormulaF f r) x
local' xs wrapEnv (Compose h) = Compose (localM xs wrapEnv h)

issueAtTrace
  :: Subtree t => Trace OpenApi t -> CheckIssue t -> CompatFormula t a
issueAtTrace xs issue = Compose $ pure $ anError $ AnItem xs issue

issueAt
  :: Subtree t
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> CompatFormula t a
issueAt f issue = Compose $ do
  xs <- asks $ getTrace . f
  pure $ anError $ AnItem xs issue
