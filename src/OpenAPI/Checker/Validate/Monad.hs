module OpenAPI.Checker.Validate.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (State, runState)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Generics.Product
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
import           Data.Sequence              (Seq)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Traversable
import           GHC.Generics               (Generic)

type TreeM n = ReaderT Env (State n)

runTreeM :: (Monoid n) => Env -> TreeM n a -> (a, n)
runTreeM env ma = runState (runReaderT ma env) mempty

data OldNew a = OldNew
  { old :: a
  , new :: a
  } deriving (Eq, Ord, Show, Generic)

instance (Semigroup a) => Semigroup (OldNew a) where
  (<>) = genericMappend

instance (Monoid a) => Monoid (OldNew a) where
  mappend = (<>)
  mempty = genericMempty

data Env = Env
  { servers    :: OldNew (Map ServerKey Server)
  , parameters :: OldNew (Map ParamKey Param)
  } deriving (Eq, Generic)

instance Semigroup Env where
  (<>) = genericMappend

instance Monoid Env where
  mappend = (<>)
  mempty = genericMempty

emptyEnv :: Env
emptyEnv = Env mempty mempty

-- | Orphan for ParamKey
deriving instance Ord ParamLocation

data ParamKey = ParamKey
  { name    :: Text
  , paramIn :: ParamLocation
  } deriving (Eq, Ord, Show, Generic)

getParamKey :: Param -> ParamKey
getParamKey p = ParamKey
  { name = _paramName p
  , paramIn = _paramIn p
  }

fromParams :: [Param] -> Map ParamKey Param
fromParams ps = M.fromList $ ps <&> \p -> (getParamKey p, p)

-- | Still not sure what identifies the server
newtype ServerKey = ServerKey Text
  deriving (Eq, Ord, Show, Generic)

getServerKey :: Server -> ServerKey
getServerKey s = ServerKey $ _serverUrl s

fromServers :: [Server] -> Map ServerKey Server
fromServers ss = M.fromList $ ss <&> \s -> (getServerKey s, s)

data Diff n
  = DiffChanged (Changed n)
  -- | Entity is presented in both trees. We need to go deeper to get more
  -- details about what changed
  | DiffFinal (Final (Original n))
  -- | Entity not presented in one of trees. We state that fact and equip with
  -- compatibility flag and comments
  deriving (Generic)

deriving instance (Eq (Changed n), Eq (Final (Original n))) => Eq (Diff n)
deriving instance (Ord (Changed n), Ord (Final (Original n))) => Ord (Diff n)
deriving instance (Show (Changed n), Show (Final (Original n))) => Show (Diff n)

-- | Two entities are presented in both trees
data Changed n = Changed
  { key    :: Key n
  -- ^ The key to find entity in both trees by
  , change :: n
  -- ^ Change description
  } deriving (Generic)

deriving instance (Eq (Key n), Eq n) => Eq (Changed n)
deriving instance (Ord (Key n), Ord n) => Ord (Changed n)
deriving instance (Show (Key n), Show n) => Show (Changed n)

chdiff :: (Applicative f) => Key n -> n -> f (Diff n)
chdiff key n = pure $ DiffChanged $ Changed key n

final :: (Applicative f) => Final (Original n) -> f (Diff n)
final = error "FIXME: final not implemented"

-- | Entity is not presented in one of trees
data Final orig = Final
  { compat   :: Compatible
  -- | Is change compatible
  , diffOp   :: DiffOp
  -- | What happened. Entity was added or deleted
  , original :: orig
  -- | The entity which was added or deleted
  } deriving (Eq, Ord, Show, Generic)

data DiffOp = Removed | Added
  deriving (Eq, Ord, Show, Generic)

data Compatible = Compatible | Incompatible Text
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Compatible where
  a <> b = case (a, b) of
    (Compatible, b)                  -> b
    (a, Compatible)                  -> a
    (Incompatible a, Incompatible b) -> Incompatible $ T.unlines [a, b]

instance Monoid Compatible where
  mappend = (<>)
  mempty = Compatible

-- | Nodes having parent Tree element
class (Monoid (Parent n)) => Node n where
  type Parent n :: *
  type Key n :: *
  type Original n :: *
  nest :: Key n -> n -> Parent n -> Parent n

followSingle
  :: (Node t, Monoid t)
  => Key t
  -> TreeM t a
  -> TreeM (Parent t) a
followSingle k mt = runIdentity <$> follow (Identity (k, mt))

-- | Runs several computations in different paths in subtrees
follow
  :: (Traversable f, Node t, Monoid t)
  => f (Key t, TreeM t a)
  -> TreeM (Parent t) (f a)
follow tree = do
  env <- ask
  for tree $ \(key, sub) -> do
    let
      (res, t) = runTreeM env sub
    modify $ nest key t
    return res

follow_
  :: (Traversable f, Node t, Monoid t)
  => f (Key t, TreeM t a)
  -> TreeM (Parent t) ()
follow_ fa = void $ follow fa
