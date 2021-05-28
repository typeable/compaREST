{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Subtree
  ( Steppable (..)
  , Step (..)
  , TraceRoot
  , Trace
  , Traced
  , Traced'
  , pattern Traced
  , traced
  , retraced
  , stepTraced
  , Subtree (..)
  , checkCompatibility
  , checkSubstructure
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

    -- * Structural helpers
  , structuralMaybe
  , structuralMaybeWith
  , structuralEq
  , iohmStructural
  , iohmStructuralWith
  , structuralList

    -- * Reexports
  , (>>>)
  , (<<<)
  , extract
  , ask
  , local
  , step
  , Typeable
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
  NiceQuiver Step a b =>
  Steppable (a :: Type) (b :: Type)
  where
  -- | How to get from an @a@ node to a @b@ node
  data Step a b :: Type

data TraceRoot

instance Steppable TraceRoot OpenApi where
  data Step TraceRoot OpenApi
    = ClientSchema
    | ServerSchema
    deriving stock (Eq, Ord, Show)

type Trace = Paths Step TraceRoot

type Traced' a b = Env (Trace a) b

type Traced a = Traced' a a

pattern Traced :: Trace a -> b -> Traced' a b
pattern Traced t x = EnvT t (Identity x)

{-# COMPLETE Traced #-}

traced :: Trace a -> a -> Traced a
traced = env

retraced :: (Trace a -> Trace a') -> Traced' a b -> Traced' a' b
retraced f (Traced a b) = Traced (f a) b

stepTraced :: Steppable a a' => Step a a' -> Traced' a b -> Traced' a' b
stepTraced s = retraced (>>> step s)

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

type StructuralCompatFormula = CompatFormula' VoidQuiver Proxy ()

data VoidQuiver a b where

deriving stock instance Eq (VoidQuiver a b)

deriving stock instance Ord (VoidQuiver a b)

deriving stock instance Show (VoidQuiver a b)

class (Typeable t, Issuable (SubtreeLevel t)) => Subtree (t :: Type) where
  type CheckEnv t :: [Type]
  type SubtreeLevel t :: BehaviorLevel

  checkStructuralCompatibility
    :: (HasAll (CheckEnv t) xs)
    => HList xs
    -> ProdCons (Traced t)
    -> StructuralCompatFormula ()

  checkSemanticCompatibility
    :: (HasAll (CheckEnv t) xs)
    => HList xs
    -> Behavior (SubtreeLevel t)
    -> ProdCons (Traced t)
    -> SemanticCompatFormula ()

{-# WARNING checkStructuralCompatibility "You should not be calling this directly. Use 'checkSubstructure'" #-}

{-# WARNING checkSemanticCompatibility "You should not be calling this directly. Use 'checkCompatibility'" #-}

checkCompatibility
  :: (HasAll (CheckEnv t) xs, Subtree t)
  => HList xs
  -> Behavior (SubtreeLevel t)
  -> ProdCons (Traced t)
  -> SemanticCompatFormula ()
checkCompatibility e bhv = memo bhv SemanticMemoKey $ \pc ->
  case runCompatFormula $ checkSubstructure e pc of
    Left _ -> checkSemanticCompatibility e bhv pc
    Right () -> pure ()

checkSubstructure
  :: (HasAll (CheckEnv t) xs, Subtree t)
  => HList xs
  -> ProdCons (Traced t)
  -> StructuralCompatFormula ()
checkSubstructure e = memo Root StructuralMemoKey $ checkStructuralCompatibility e

structuralMaybe
  :: (Subtree a, HasAll (CheckEnv a) xs)
  => HList xs
  -> ProdCons (Maybe (Traced a))
  -> StructuralCompatFormula ()
structuralMaybe e = structuralMaybeWith (checkSubstructure e)

structuralMaybeWith
  :: (ProdCons a -> StructuralCompatFormula ())
  -> ProdCons (Maybe a)
  -> StructuralCompatFormula ()
structuralMaybeWith f (ProdCons (Just a) (Just b)) = f $ ProdCons a b
structuralMaybeWith _ (ProdCons Nothing Nothing) = pure ()
structuralMaybeWith _ _ = structuralIssue

structuralList
  :: (Subtree a, HasAll (CheckEnv a) xs)
  => HList xs
  -> ProdCons [Traced a]
  -> StructuralCompatFormula ()
structuralList _ (ProdCons [] []) = pure ()
structuralList e (ProdCons (a : aa) (b : bb)) = do
  checkSubstructure e $ ProdCons a b
  structuralList e $ ProdCons aa bb
  pure ()
structuralList _ _ = structuralIssue

structuralEq :: (Eq a, Comonad w) => ProdCons (w a) -> StructuralCompatFormula ()
structuralEq (ProdCons a b) = if extract a == extract b then pure () else structuralIssue

iohmStructural
  :: (HasAll (CheckEnv v) (k ': xs), Ord k, Subtree v, Hashable k, Typeable k, Show k)
  => HList xs
  -> ProdCons (Traced (IOHM.InsOrdHashMap k v))
  -> StructuralCompatFormula ()
iohmStructural e =
  iohmStructuralWith (\k -> checkSubstructure (k `HCons` e))

instance (Typeable k, Typeable v, Ord k, Show k) => Steppable (IOHM.InsOrdHashMap k v) v where
  data Step (IOHM.InsOrdHashMap k v) v = InsOrdHashMapKeyStep k
    deriving (Eq, Ord, Show)

iohmStructuralWith
  :: (Ord k, Hashable k, Typeable k, Typeable v, Show k)
  => (k -> ProdCons (Traced v) -> StructuralCompatFormula ())
  -> ProdCons (Traced (IOHM.InsOrdHashMap k v))
  -> StructuralCompatFormula ()
iohmStructuralWith f pc = do
  let ProdCons pEKeys cEKeys = S.fromList . IOHM.keys . extract <$> pc
  if pEKeys == cEKeys
    then
      for_
        pEKeys
        (\eKey ->
           f eKey $ stepTraced (InsOrdHashMapKeyStep eKey) . fmap (IOHM.lookupDefault (error "impossible") eKey) <$> pc)
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
structuralIssue = Compose $ pure $ anError $ AnItem Root Proxy

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
  :: (Typeable (l :: k), Typeable q, Typeable f, Typeable k, Typeable a)
  => Paths q r l
  -> MemoKey
  -> (ProdCons (Traced a) -> CompatFormula' q f r ())
  -> (ProdCons (Traced a) -> CompatFormula' q f r ())
memo bhv k f pc = Compose $ do
  formula' <- memoWithKnot fixpointKnot (do
    formula <- getCompose $ f pc
    pure $ mapErrors (P.filter bhv) formula
    ) (k, ask <$> pc)
  pure $ mapErrors (P.embed bhv) formula'

data MemoKey = SemanticMemoKey | StructuralMemoKey
  deriving stock (Eq, Ord)
