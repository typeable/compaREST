module OpenAPI.Checker.Validate.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import           Data.Functor.Identity
import           Data.OpenApi.Internal
import           Data.Text                        (Text)
import           Data.Traversable
import           GHC.Generics                     (Generic)

type TreeM t = ReaderT Env (StateT t Errorable)

data Env = Env
  { oldServers :: [Server]
  , newServers :: [Server]
  } deriving (Eq, Generic)

emptyEnv :: Env
emptyEnv = Env [] []

-- | Might be different type in future
type Errorable = Either Err

type Err = Text

-- | Class of trees nested into another trees
class (Monoid (Parent t)) => Nested t where
  type Parent t
  type Key t
  nest :: Key t -> Errorable t -> Parent t

runTreeM :: (Monoid t) => Env -> TreeM t a -> Errorable (a, t)
runTreeM env ma = runStateT (runReaderT ma env) mempty

-- | Throws error in current tree
treeError :: Err -> TreeM t a
treeError = throwError

-- | Runs several computations in different paths in subtrees
follow
  :: (Foldable f, Nested t, Monoid t)
  => f (Key t, TreeM t a)
  -> TreeM (Parent t) ()
follow tree = do
  env <- ask
  for_ tree $ \(key, sub) -> do
    let p = nest key $ fmap snd $ runTreeM env sub
    id <>= p
