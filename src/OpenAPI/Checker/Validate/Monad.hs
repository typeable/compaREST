module OpenAPI.Checker.Validate.Monad where

import           Data.Text (Text)


data TreeM t a = TreeM
instance Functor (TreeM t)
instance Applicative (TreeM t)
instance Monad (TreeM t)

type Errorable = Either Text

-- | Class of trees nested into another trees
class Nested t where
  type Parent t
  type Key t
  nest :: Key t -> Errorable t -> Parent t

runTreeM :: TreeM t a -> (t, a)
runTreeM = error "FIXME: runReportTree not implemented"

pathError :: Text -> TreeM t a
pathError = error "FIXME: pathError not implemented"

-- | Runs several computations in different paths
follow
  :: (Traversable f, Nested t)
  => f (Key t, TreeM t a)
  -> TreeM (Parent t) (f a)
follow = error "FIXME: follow not implemented"
