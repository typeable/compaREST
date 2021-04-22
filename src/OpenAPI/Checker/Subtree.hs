module OpenAPI.Checker.Subtree
  ( APIStep (..)
  , Subtree (..)
  , CompatM (..)
  , CompatFormula'
  , CompatFormula
  , ProdCons (..)
  , prodConsAccessors
  , Accessor (..)
  , SomeIssue (..)
  , issue
  , HasUnsupportedFeature (..)
  , swapProdCons
  , SubtreeCheckIssue (..)
  , runCompatFormula
  , anyOfM
  , anyOfAt
  , anyOfSubtreeAt
  , issueAtTrace
  , issueAt
  , memo
  )
where

import Control.Comonad.Env
import Control.Monad.Identity
import Control.Monad.State
import Data.Aeson
import Data.Functor.Compose
import Data.HList
import Data.Kind
import Data.Monoid
import Data.OpenApi
import Data.Text
import Data.Typeable
import OpenAPI.Checker.Formula
import OpenAPI.Checker.Memo
import OpenAPI.Checker.Trace
import qualified OpenAPI.Checker.TracePrefixTree as T

class
  (Subtree a, Subtree b, Steppable a b) =>
  APIStep (a :: Type) (b :: Type)
  where
  describeStep :: Step a b -> Text

data ProdCons a = ProdCons
  { producer :: a
  , consumer :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Accessor f = Accessor (forall x. f x -> x)

prodConsAccessors :: ProdCons (Accessor ProdCons)
prodConsAccessors =
  ProdCons
    { producer = Accessor producer
    , consumer = Accessor consumer
    }

swapProdCons :: ProdCons a -> ProdCons a
swapProdCons (ProdCons a b) = ProdCons b a

instance Applicative ProdCons where
  pure x = ProdCons x x
  ProdCons fp fc <*> ProdCons xp xc = ProdCons (fp xp) (fc xc)

newtype CompatM a = CompatM
  { unCompatM
    :: (StateT (MemoState VarRef) Identity) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (MemoState VarRef)
    )

type CompatFormula' f r = Compose CompatM (FormulaF f r)
type CompatFormula = CompatFormula' SubtreeCheckIssue OpenApi

class (Typeable t, Ord (CheckIssue t), Show (CheckIssue t)) => Subtree (t :: Type) where
  type CheckEnv t :: [Type]
  data CheckIssue t :: Type

  issueIsUnsupported :: CheckIssue t -> Bool
  issueIsUnsupported = const False

  -- | If we ever followed a reference, reroute the path through "components"
  normalizeTrace :: Trace OpenApi t -> Trace OpenApi t
  normalizeTrace = id

  checkCompatibility
    :: HasAll (CheckEnv t) xs
    => HList xs
    -> ProdCons (Traced OpenApi t)
    -> CompatFormula ()

{-# WARNING normalizeTrace "It must be refactored. Does nothing for now" #-}

class HasUnsupportedFeature x where
  hasUnsupportedFeature :: x -> Bool

instance HasUnsupportedFeature () where
  hasUnsupportedFeature () = False

instance
  (HasUnsupportedFeature a, HasUnsupportedFeature b)
  => HasUnsupportedFeature (Either a b)
  where
  hasUnsupportedFeature (Left x) = hasUnsupportedFeature x
  hasUnsupportedFeature (Right x) = hasUnsupportedFeature x

instance Subtree t => HasUnsupportedFeature (CheckIssue t) where
  hasUnsupportedFeature = issueIsUnsupported

instance
  (forall x. HasUnsupportedFeature (f x))
  => HasUnsupportedFeature (T.TracePrefixTree f r)
  where
  hasUnsupportedFeature =
    getAny . T.foldWith (\_ fa -> Any $ hasUnsupportedFeature fa)

data SubtreeCheckIssue t where
  SubtreeCheckIssue :: Subtree t => CheckIssue t -> SubtreeCheckIssue t

deriving stock instance Eq (SubtreeCheckIssue t)

deriving stock instance Ord (SubtreeCheckIssue t)

instance Subtree t => ToJSON (CheckIssue t) where
  toJSON = toJSON . show

instance HasUnsupportedFeature (SubtreeCheckIssue t) where
  hasUnsupportedFeature (SubtreeCheckIssue i) = hasUnsupportedFeature i

instance ToJSON (SubtreeCheckIssue t) where
  toJSON (SubtreeCheckIssue i) = toJSON i

runCompatFormula
  :: CompatFormula' f r a
  -> Either (T.TracePrefixTree f r) a
runCompatFormula (Compose f) =
  calculate . runIdentity . runMemo 0 . unCompatM $ f

issueAtTrace
  :: Subtree t
  => Trace r t
  -> CheckIssue t
  -> CompatFormula' SubtreeCheckIssue r a
issueAtTrace xs issue = Compose $ pure $ anError $ AnItem xs $ SubtreeCheckIssue issue

issueAt
  :: (Subtree t, ComonadEnv (Trace r t) w)
  => w x
  -> CheckIssue t
  -> CompatFormula' SubtreeCheckIssue r a
issueAt x = issueAtTrace (ask x)

anyOfM
  :: Subtree t
  => Trace r t
  -> CheckIssue t
  -> [CompatFormula' SubtreeCheckIssue r a]
  -> CompatFormula' SubtreeCheckIssue r a
anyOfM xs issue fs =
  Compose $ (`eitherOf` AnItem xs (SubtreeCheckIssue issue)) <$> sequenceA (getCompose <$> fs)

anyOfAt
  :: (Subtree t, ComonadEnv (Trace r t) w)
  => w x
  -> CheckIssue t
  -> [CompatFormula' SubtreeCheckIssue r a]
  -> CompatFormula' SubtreeCheckIssue r a
anyOfAt x = anyOfM (ask x)

fixpointKnot
  :: MonadState (MemoState VarRef) m
  => KnotTier (FormulaF f r ()) VarRef m
fixpointKnot =
  KnotTier
    { onKnotFound = modifyMemoNonce succ
    , onKnotUsed = \i -> pure $ variable i
    , tieKnot = \i x -> pure $ maxFixpoint i x
    }

memo
  :: (Typeable r, Subtree t)
  => (ProdCons (Traced r t) -> CompatFormula ())
  -> (ProdCons (Traced r t) -> CompatFormula ())
memo f pc  = Compose $ do
  memoWithKnot fixpointKnot (getCompose $ f pc) (ask <$> pc)
