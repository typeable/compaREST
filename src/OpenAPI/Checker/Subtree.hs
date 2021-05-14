{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Subtree
  ( Steppable (..)
  , Trace
  , Traced
  , Traced'
  , pattern Traced
  , traced
  , Subtree (..)
  , checkCompatibility
  , CompatM (..)
  , CompatFormula'
  , SemanticCompatFormula
  , ProdCons (..)
  , HasUnsupportedFeature (..)
  , swapProdCons
  , runCompatFormula
  , issueAt
  , anyOfAt
  , structuralIssue
  , memo

    -- * Structural helpers
  , structuralMaybe
  , structuralEq
  , iohmStructuralCompatibility

    -- * Reexports
  , (>>>)
  , (<<<)
  , extract
  , ask
  , local
  , step
  )
where

import Control.Comonad.Env
import Control.Monad.Identity
import Control.Monad.State
import Data.Foldable
import Data.Functor.Compose
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Hashable
import Data.Kind
import Data.Monoid
import Data.OpenApi
import qualified Data.Set as S
import Data.Typeable
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Formula
import OpenAPI.Checker.Memo
import OpenAPI.Checker.Paths
import qualified OpenAPI.Checker.PathsPrefixTree as P

class
  (Typeable a, Typeable b, Ord (Step a b), Show (Step a b)) =>
  Steppable (a :: Type) (b :: Type)
  where
  -- | How to get from an @a@ node to a @b@ node
  data Step a b :: Type

type Trace = Paths Step OpenApi

type Traced' a b = Env (Trace a) b

type Traced a = Traced' a a

pattern Traced :: Trace a -> b -> Traced' a b
pattern Traced t x = EnvT t (Identity x)

{-# COMPLETE Traced #-}

traced :: Trace a -> a -> Traced a
traced = env

data ProdCons a = ProdCons
  { producer :: a
  , consumer :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

swapProdCons :: ProdCons a -> ProdCons a
swapProdCons (ProdCons a b) = ProdCons b a

instance Applicative ProdCons where
  pure x = ProdCons x x
  ProdCons fp fc <*> ProdCons xp xc = ProdCons (fp xp) (fc xc)

newtype CompatM a = CompatM
  { unCompatM
    :: StateT (MemoState VarRef) Identity a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (MemoState VarRef)
    )

type CompatFormula' q f r = Compose CompatM (FormulaF q f r)

type SemanticCompatFormula = CompatFormula' Behave AnIssue 'APILevel

type StructuralCompatFormula = CompatFormula' UnitQuiver Proxy ()

data UnitQuiver a b where
  UnitQuiver :: UnitQuiver () ()

deriving stock instance Eq (UnitQuiver a b)

deriving stock instance Ord (UnitQuiver a b)

deriving stock instance Show (UnitQuiver a b)

class (Typeable t, Issuable (SubtreeLevel t)) => Subtree (t :: Type) where
  type CheckEnv t :: [Type]
  type SubtreeLevel t :: BehaviorLevel

  checkStructuralCompatibility
    :: (HasAll (CheckEnv t) xs)
    => HList xs
    -> ProdCons t
    -> StructuralCompatFormula ()

  checkSemanticCompatibility
    :: (HasAll (CheckEnv t) xs)
    => HList xs
    -> Behavior (SubtreeLevel t)
    -> ProdCons (Traced t)
    -> SemanticCompatFormula ()

checkCompatibility
  :: (HasAll (CheckEnv t) xs, Subtree t)
  => HList xs
  -> Behavior (SubtreeLevel t)
  -> ProdCons (Traced t)
  -> SemanticCompatFormula ()
checkCompatibility e bhv pc =
  case runCompatFormula $ checkStructuralCompatibility e $ fmap extract pc of
    Left _ -> checkSemanticCompatibility e bhv pc
    Right () -> pure ()

structuralMaybe
  :: (Subtree a, HasAll (CheckEnv a) xs)
  => HList xs
  -> ProdCons (Maybe a)
  -> StructuralCompatFormula ()
structuralMaybe e (ProdCons (Just a) (Just b)) = checkStructuralCompatibility e $ ProdCons a b
structuralMaybe _ (ProdCons Nothing Nothing) = pure ()
structuralMaybe _ _ = structuralIssue

structuralEq :: Eq a => ProdCons a -> StructuralCompatFormula ()
structuralEq (ProdCons a b) = if a == b then pure () else structuralIssue

iohmStructuralCompatibility
  :: (HasAll (CheckEnv v) (k ': xs), Ord k, Subtree v, Hashable k)
  => HList xs
  -> ProdCons (IOHM.InsOrdHashMap k v)
  -> StructuralCompatFormula ()
iohmStructuralCompatibility e pc = do
  let ProdCons pEKeys cEKeys = S.fromList . IOHM.keys <$> pc
  if pEKeys == cEKeys
    then
      for_
        pEKeys
        (\eKey ->
           checkStructuralCompatibility (eKey `HCons` e) $
             IOHM.lookupDefault (error "impossible") eKey <$> pc)
    else structuralIssue

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

instance Issuable l => HasUnsupportedFeature (Issue l) where
  hasUnsupportedFeature = issueIsUnsupported

instance HasUnsupportedFeature (AnIssue l) where
  hasUnsupportedFeature (AnIssue issue) = hasUnsupportedFeature issue

instance
  (forall x. HasUnsupportedFeature (f x))
  => HasUnsupportedFeature (P.PathsPrefixTree q f r)
  where
  hasUnsupportedFeature =
    getAny . P.foldWith (\_ fa -> Any $ hasUnsupportedFeature fa)

runCompatFormula
  :: CompatFormula' q f r a
  -> Either (P.PathsPrefixTree q f r) a
runCompatFormula (Compose f) =
  calculate . runIdentity . runMemo 0 . unCompatM $ f

issueAt :: Issuable l => Paths q r l -> Issue l -> CompatFormula' q AnIssue r a
issueAt xs issue = Compose $ pure $ anError $ AnItem xs $ AnIssue issue

structuralIssue :: StructuralCompatFormula a
structuralIssue = Compose $ pure $ anError $ AnItem (step UnitQuiver) Proxy

anyOfAt
  :: Issuable l
  => Paths q r l
  -> Issue l
  -> [CompatFormula' q AnIssue r a]
  -> CompatFormula' q AnIssue r a
anyOfAt _ _ [x] = x
anyOfAt xs issue fs =
  Compose $ (`eitherOf` AnItem xs (AnIssue issue)) <$> sequenceA (getCompose <$> fs)

fixpointKnot
  :: MonadState (MemoState VarRef) m
  => KnotTier (FormulaF q f r ()) VarRef m
fixpointKnot =
  KnotTier
    { onKnotFound = modifyMemoNonce succ
    , onKnotUsed = \i -> pure $ variable i
    , tieKnot = \i x -> pure $ maxFixpoint i x
    }

memo
  :: (Typeable q, Typeable f, NiceQuiver p r t)
  => (ProdCons (Env (Paths p r t) t) -> CompatFormula' q f r ())
  -> (ProdCons (Env (Paths p r t) t) -> CompatFormula' q f r ())
memo f pc = Compose $ do
  memoWithKnot fixpointKnot (getCompose $ f pc) (ask <$> pc)
