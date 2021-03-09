module OpenAPI.Checker.Subtree
  ( APIStep(..)
  , CompatibilityMonad
  , Subtree(..)
  , local'
  , warnIssue
  , throwIssue
  , warnIssueAt
  , throwIssueAt
  , throwWarnings
  , warnIssues
  ) where

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import Control.Monad.Writer.Class
import Data.Kind
import Data.OpenApi
import Data.Text
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

class (Subtree a, Subtree b, Steppable a b)
  => APIStep (a :: Type) (b :: Type) where
  describeStep :: Step a b -> Text

type CompatibilityMonad m =
  ( MonadWriter (T.TracePrefixTree CheckIssue OpenApi) m
  , MonadError (T.TracePrefixTree CheckIssue OpenApi) m
  )

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

class Ord (CheckIssue t) => Subtree (t :: Type) where
  type family CheckEnv t :: Type
  data family CheckIssue t :: Type
  checkCompatibility
    :: CompatibilityMonad m
    => ProdCons t -> ReaderT (ProdCons (TracedEnv t)) m ()

local'
  :: Monad m
  => ProdCons (Trace a b)
  -> (ProdCons (CheckEnv a) -> ProdCons (CheckEnv b))
  -> ReaderT (ProdCons (TracedEnv b)) m x
  -> ReaderT (ProdCons (TracedEnv a)) m x
local' xs wrapEnv k = do
  env <- ask
  lift $ runReaderT k $ TracedEnv
    <$> (catTrace <$> (getTrace <$> env) <*> xs)
    <*> wrapEnv (getEnv <$> env)

warnIssue
  :: (Subtree t, CompatibilityMonad m)
  => Trace OpenApi t -> CheckIssue t -> m ()
warnIssue xs issue = tell $ T.singleton $ AnItem xs issue

throwIssue
  :: (Subtree t, CompatibilityMonad m)
  => Trace OpenApi t -> CheckIssue t -> m a
throwIssue xs issue = throwError $ T.singleton $ AnItem xs issue

warnIssueAt
  :: (Subtree t, CompatibilityMonad m)
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> ReaderT (ProdCons (TracedEnv t)) m ()
warnIssueAt f issue = do
  xs <- asks $ getTrace . f
  warnIssue xs issue

throwIssueAt
  :: (Subtree t, CompatibilityMonad m)
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> ReaderT (ProdCons (TracedEnv t)) m a
throwIssueAt f issue = do
  xs <- asks $ getTrace . f
  throwIssue xs issue

throwWarnings :: CompatibilityMonad m => m a -> m a
throwWarnings k = listen k >>= \case
  (x, issues) -> if T.null issues then pure x else throwError issues

warnIssues :: CompatibilityMonad m => m a -> m (Maybe a)
warnIssues k = catchError (Just <$> k) (\issues -> Nothing <$ tell issues)
