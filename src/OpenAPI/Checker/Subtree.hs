module OpenAPI.Checker.Subtree
  ( APIStep(..)
  , Subtree(..)
  , CompatM(..)
  , runCompatM
  , local'
  , warnIssue
  , throwIssue
  , warnIssueAt
  , throwIssueAt
  , throwWarnings
  , warnIssues
  ) where

import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import Control.Monad.Writer.Class
import Data.Kind
import Data.OpenApi
import Data.Text
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
      (ExceptT (T.TracePrefixTree CheckIssue OpenApi)
        (WriterT (T.TracePrefixTree CheckIssue OpenApi)
          (StateT MemoState Identity))) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (ProdCons (TracedEnv t))
    , MonadError (T.TracePrefixTree CheckIssue OpenApi)
    , MonadWriter (T.TracePrefixTree CheckIssue OpenApi)
    )

class (Ord t, Ord (CheckIssue t)) => Subtree (t :: Type) where
  type family CheckEnv t :: Type
  data family CheckIssue t :: Type
  checkCompatibility :: ProdCons t -> CompatM t a

runCompatM :: ProdCons (TracedEnv t) -> CompatM t a ->
  ( Either (T.TracePrefixTree CheckIssue OpenApi) a -- unrecovarable errors
  , T.TracePrefixTree CheckIssue OpenApi ) -- warnings
runCompatM env = runIdentity . runMemo . runWriterT
  . runExceptT . (`runReaderT` env) . unCompatM

local'
  :: ProdCons (Trace a b)
  -> (ProdCons (CheckEnv a) -> ProdCons (CheckEnv b))
  -> CompatM b x
  -> CompatM a x
local' xs wrapEnv (CompatM k) = CompatM $ ReaderT $ \env ->
  runReaderT k $ TracedEnv
    <$> (catTrace <$> (getTrace <$> env) <*> xs)
    <*> wrapEnv (getEnv <$> env)

warnIssue :: Subtree t => Trace OpenApi t -> CheckIssue t -> CompatM t ()
warnIssue xs issue = tell $ T.singleton $ AnItem xs issue

throwIssue :: Subtree t => Trace OpenApi t -> CheckIssue t -> CompatM t a
throwIssue xs issue = throwError $ T.singleton $ AnItem xs issue

warnIssueAt
  :: Subtree t
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> CompatM t ()
warnIssueAt f issue = do
  xs <- asks $ getTrace . f
  warnIssue xs issue

throwIssueAt
  :: Subtree t
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> CompatM t a
throwIssueAt f issue = do
  xs <- asks $ getTrace . f
  throwIssue xs issue

throwWarnings :: CompatM t a -> CompatM t a
throwWarnings k = listen k >>= \case
  (x, issues) -> if T.null issues then pure x else throwError issues

warnIssues :: CompatM t a -> CompatM t (Maybe a)
warnIssues k = catchError (Just <$> k) (\issues -> Nothing <$ tell issues)
