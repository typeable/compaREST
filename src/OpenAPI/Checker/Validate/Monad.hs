module OpenAPI.Checker.Validate.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Generics.Product
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
import           Data.Sequence                    (Seq)
import           Data.Text                        (Text)
import           Data.Traversable
import           GHC.Generics                     (Generic)

type TreeM n = ReaderT Env (State (Seq (Diff n)))

data Env = Env
  { oldServers :: [Server]
  , newServers :: [Server]
  } deriving (Eq, Generic)

emptyEnv :: Env
emptyEnv = Env [] []

-- | Any diff may be compatible or incompatible, so we split this information
-- into two different fields.
data Diff n = Diff
  { compat :: Compatible
  , diff   :: DiffDesc n
  } deriving (Generic)

deriving instance (Eq (DiffDesc n)) => Eq (Diff n)
deriving instance (Ord (DiffDesc n)) => Ord (Diff n)
deriving instance (Show (DiffDesc n)) => Show (Diff n)

data Compatible = Compatible | Uncompatible
  deriving (Eq, Ord, Show, Generic)

data DiffDesc n
  = Removed Text (Original n)
  -- ^ Entity removed in new version. We don't need the key here, because the
  -- path exists only in one of compared trees
  | Added Text (Original n)
  -- ^ Entity added in new version. We don't need the key here, because the path
  -- exists only in one of compared trees
  | Changed (Key n) n
  -- ^ We need the key to determine the path in the tree we are going to.
  deriving (Generic)

deriving instance (Eq n, Eq (Original n), Eq (Key n)) => Eq (DiffDesc n)
deriving instance (Ord n, Ord (Original n), Ord (Key n)) => Ord (DiffDesc n)
deriving instance (Show n, Show (Original n), Show (Key n)) => Show (DiffDesc n)

-- | Nodes having parent Tree element
class (Monoid (Parent n)) => Node n where
  type Parent n :: *
  type Key n :: *
  type Original n :: *
  nest :: Seq (Diff n) -> Parent n

runTreeM :: Env -> TreeM n a -> (a, Seq (Diff n))
runTreeM env ma = runState (runReaderT ma env) mempty

diffCompat :: DiffDesc n -> TreeM n ()
diffCompat desc = do
  let dif = pure $ Diff Compatible desc
  id <>= dif

diffUncompat :: DiffDesc n -> TreeM n ()
diffUncompat desc = do
  let dif = pure $ Diff Uncompatible desc
  id <>= dif


-- | Runs several computations in different paths in subtrees
follow
  :: (Traversable f, Node t, Monoid t)
  => f (Key t, TreeM t a)
  -> TreeM (Parent t) (f a)
follow tree = do
  env <- ask
  for tree $ \(key, sub) -> do
    let
      (res, seq) = runTreeM env sub
      p = nest seq
    id <>= p
    return res
