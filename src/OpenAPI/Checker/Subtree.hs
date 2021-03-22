module OpenAPI.Checker.Subtree
  ( APIStep (..)
  , Subtree (..)
  , CompatM (..)
  , CompatFormula
  , ProdCons (..)
  , HasUnsupportedFeature (..)
  , runCompatFormula
  , localM
  , localTrace
  , anyOfM
  , anyOfAt
  , issueAtTrace
  , issueAt
  , memo
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
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

instance Applicative ProdCons where
  pure x = ProdCons x x
  ProdCons fp fc <*> ProdCons xp xc = ProdCons (fp xp) (fc xc)

newtype CompatM t a = CompatM
  { unCompatM
    :: ReaderT
           (ProdCons (Trace OpenApi t))
           (StateT (MemoState VarRef) Identity)
           a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ProdCons (Trace OpenApi t))
    , MonadState (MemoState VarRef)
    )

type CompatFormula t = Compose (CompatM t) (FormulaF SomeCheckIssue OpenApi)

class (Typeable t, Ord (CheckIssue t), Show (CheckIssue t)) => Subtree (t :: Type) where
  type CheckEnv t :: [Type]
  data CheckIssue t :: Type

  issueIsUnsupported :: CheckIssue t -> Bool
  issueIsUnsupported = const False

  -- | If we ever followed a reference, reroute the path through "components"
  normalizeTrace :: Trace OpenApi t -> Trace OpenApi t

  checkCompatibility :: HasAll (CheckEnv t) xs => HList xs -> ProdCons t -> CompatFormula t ()

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

data SomeCheckIssue t where
  SomeCheckIssue :: Subtree t => CheckIssue t -> SomeCheckIssue t

deriving stock instance Eq (SomeCheckIssue t)

deriving stock instance Ord (SomeCheckIssue t)

instance Subtree t => ToJSON (CheckIssue t) where
  toJSON = toJSON . show

instance HasUnsupportedFeature (SomeCheckIssue t) where
  hasUnsupportedFeature (SomeCheckIssue i) = hasUnsupportedFeature i

instance ToJSON (SomeCheckIssue t) where
  toJSON (SomeCheckIssue i) = toJSON i

runCompatFormula
  :: ProdCons (Trace OpenApi t)
  -> Compose (CompatM t) (FormulaF f r) a
  -> Either (T.TracePrefixTree f r) a
runCompatFormula env (Compose f) =
  calculate . runIdentity . runMemo 0 . (`runReaderT` env) . unCompatM $ f

localM
  :: ProdCons (Trace a b)
  -> CompatM b x
  -> CompatM a x
localM xs (CompatM k) =
  CompatM $ ReaderT $ \env -> runReaderT k ((>>>) <$> env <*> xs)

localTrace
  :: ProdCons (Trace a b)
  -> Compose (CompatM b) (FormulaF f r) x
  -> Compose (CompatM a) (FormulaF f r) x
localTrace xs (Compose h) = Compose (localM xs h)

issueAtTrace
  :: Subtree t => Trace OpenApi t -> CheckIssue t -> CompatFormula t a
issueAtTrace xs issue = Compose $ pure $ anError $ AnItem xs $ SomeCheckIssue issue

issueAt
  :: Subtree t
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> CompatFormula t a
issueAt f issue = Compose $ do
  xs <- asks f
  pure $ anError $ AnItem xs $ SomeCheckIssue issue

anyOfM
  :: Ord (f t)
  => Trace r t
  -> f t
  -> [Compose (CompatM t) (FormulaF f r) a]
  -> Compose (CompatM t) (FormulaF f r) a
anyOfM xs issue fs =
  Compose $ (`eitherOf` AnItem xs issue) <$> sequenceA (getCompose <$> fs)

anyOfAt
  :: Subtree t
  => (forall x. ProdCons x -> x)
  -> CheckIssue t
  -> [CompatFormula t a]
  -> CompatFormula t a
anyOfAt f issue fs = Compose $ do
  xs <- asks f
  (`eitherOf` AnItem xs (SomeCheckIssue issue)) <$> sequenceA (getCompose <$> fs)

fixpointKnot
  :: MonadState (MemoState VarRef) m
  => KnotTier (FormulaF f r ()) VarRef m
fixpointKnot =
  KnotTier
    { onKnotFound = modifyMemoNonce succ
    , onKnotUsed = \i -> pure $ variable i
    , tieKnot = \i x -> pure $ maxFixpoint i x
    }

memo :: Subtree t => CompatFormula t () -> CompatFormula t ()
memo (Compose f) = Compose $ do
  pxs <- asks (fmap normalizeTrace)
  memoWithKnot fixpointKnot f pxs
