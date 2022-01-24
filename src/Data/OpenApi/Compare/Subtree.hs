{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Subtree
  ( Steppable (..),
    Step (..),
    TraceRoot,
    Trace,
    Traced,
    Traced',
    pattern Traced,
    traced,
    retraced,
    stepTraced,
    Subtree (..),
    checkCompatibility,
    checkSubstructure,
    CompatM (..),
    CompatFormula',
    SemanticCompatFormula,
    ProdCons (..),
    orientProdCons,
    swapProdCons,
    runCompatFormula,
    issueAt,
    anItem,
    anIssue,
    invertIssueOrientation,
    invertIssueOrientationP,
    embedFormula,
    anyOfAt,
    clarifyIssue,
    structuralIssue,

    -- * Structural helpers
    structuralMaybe,
    structuralMaybeWith,
    structuralEq,
    iohmStructural,
    iohmStructuralWith,
    structuralList,

    -- * Reexports
    (>>>),
    (<<<),
    extract,
    ask,
    local,
    step,
    Typeable,
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
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Formula
import Data.OpenApi.Compare.Memo
import Data.OpenApi.Compare.Paths
import qualified Data.OpenApi.Compare.PathsPrefixTree as P
import qualified Data.Set as S
import Data.Typeable

class
  (Typeable Step, Typeable a, Typeable b, Ord (Step a b), Show (Step a b)) =>
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

type instance AdditionalQuiverConstraints Step _ _ = ()

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
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

orientProdCons :: Orientation -> ProdCons x -> ProdCons x
orientProdCons Forward x = x
orientProdCons Backward (ProdCons p c) = ProdCons c p

swapProdCons ::
  SwapEnvRoles xs =>
  (HList xs -> ProdCons x -> CompatFormula' q AnIssue r a) ->
  (HList xs -> ProdCons x -> CompatFormula' q AnIssue r a)
swapProdCons f e (ProdCons p c) =
  invertIssueOrientation $
    f (swapEnvRoles e) (ProdCons c p)
{-# INLINE swapProdCons #-}

type family IsProdCons (x :: Type) :: Bool where
  IsProdCons (ProdCons _) = 'True
  IsProdCons _ = 'False

type SwapEnvElementRoles x = SwapEnvElementRoles' x (IsProdCons x)

class IsProdCons x ~ f => SwapEnvElementRoles' (x :: Type) f where
  swapEnvElementRoles :: x -> x

instance SwapEnvElementRoles' (ProdCons x) 'True where
  swapEnvElementRoles (ProdCons p c) = ProdCons c p

instance IsProdCons x ~ 'False => SwapEnvElementRoles' x 'False where
  swapEnvElementRoles = id

class SwapEnvRoles xs where
  swapEnvRoles :: HList xs -> HList xs

instance SwapEnvRoles '[] where
  swapEnvRoles = id

instance (SwapEnvElementRoles x, SwapEnvRoles xs) => SwapEnvRoles (x ': xs) where
  swapEnvRoles (HCons x xs) = HCons (swapEnvElementRoles x) (swapEnvRoles xs)

instance Applicative ProdCons where
  pure x = ProdCons x x
  ProdCons fp fc <*> ProdCons xp xc = ProdCons (fp xp) (fc xc)

newtype CompatM a = CompatM
  { unCompatM ::
      StateT (MemoState VarRef) Identity a
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

data VoidQuiver a b

deriving stock instance Eq (VoidQuiver a b)

type instance AdditionalQuiverConstraints VoidQuiver _ _ = ()

deriving stock instance Ord (VoidQuiver a b)

deriving stock instance Show (VoidQuiver a b)

class (Typeable t, Issuable (SubtreeLevel t)) => Subtree (t :: Type) where
  type CheckEnv t :: [Type]
  type SubtreeLevel t :: BehaviorLevel

  checkStructuralCompatibility ::
    HList (CheckEnv t) ->
    ProdCons (Traced t) ->
    StructuralCompatFormula ()

  checkSemanticCompatibility ::
    HList (CheckEnv t) ->
    Behavior (SubtreeLevel t) ->
    ProdCons (Traced t) ->
    SemanticCompatFormula ()

{-# WARNING checkStructuralCompatibility "You should not be calling this directly. Use 'checkSubstructure'" #-}

{-# WARNING checkSemanticCompatibility "You should not be calling this directly. Use 'checkCompatibility'" #-}

checkCompatibility ::
  forall t xs.
  (ReassembleHList xs (CheckEnv t), Subtree t) =>
  Behavior (SubtreeLevel t) ->
  HList xs ->
  ProdCons (Traced t) ->
  SemanticCompatFormula ()
checkCompatibility bhv e = memo bhv SemanticMemoKey $ \pc ->
  case runCompatFormula $ checkSubstructure e pc of
    Left _ -> checkSemanticCompatibility (reassemble e) bhv pc
    Right () -> pure ()
{-# INLINE checkCompatibility #-}

checkSubstructure ::
  (ReassembleHList xs (CheckEnv t), Subtree t) =>
  HList xs ->
  ProdCons (Traced t) ->
  StructuralCompatFormula ()
checkSubstructure e = memo Root StructuralMemoKey $ checkStructuralCompatibility (reassemble e)
{-# INLINE checkSubstructure #-}

structuralMaybe ::
  (Subtree a, ReassembleHList xs (CheckEnv a)) =>
  HList xs ->
  ProdCons (Maybe (Traced a)) ->
  StructuralCompatFormula ()
structuralMaybe e = structuralMaybeWith (checkSubstructure e)
{-# INLINE structuralMaybe #-}

structuralMaybeWith ::
  (ProdCons a -> StructuralCompatFormula ()) ->
  ProdCons (Maybe a) ->
  StructuralCompatFormula ()
structuralMaybeWith f (ProdCons (Just a) (Just b)) = f $ ProdCons a b
structuralMaybeWith _ (ProdCons Nothing Nothing) = pure ()
structuralMaybeWith _ _ = structuralIssue
{-# INLINE structuralMaybeWith #-}

structuralList ::
  (Subtree a, ReassembleHList xs (CheckEnv a)) =>
  HList xs ->
  ProdCons [Traced a] ->
  StructuralCompatFormula ()
structuralList _ (ProdCons [] []) = pure ()
structuralList e (ProdCons (a : aa) (b : bb)) = do
  checkSubstructure e $ ProdCons a b
  structuralList e $ ProdCons aa bb
  pure ()
structuralList _ _ = structuralIssue
{-# INLINE structuralList #-}

structuralEq :: (Eq a, Comonad w) => ProdCons (w a) -> StructuralCompatFormula ()
structuralEq (ProdCons a b) = if extract a == extract b then pure () else structuralIssue
{-# INLINE structuralEq #-}

iohmStructural ::
  (ReassembleHList (k ': xs) (CheckEnv v), Ord k, Subtree v, Hashable k, Typeable k, Show k) =>
  HList xs ->
  ProdCons (Traced (IOHM.InsOrdHashMap k v)) ->
  StructuralCompatFormula ()
iohmStructural e =
  iohmStructuralWith (\k -> checkSubstructure (k `HCons` e))
{-# INLINE iohmStructural #-}

instance (Typeable k, Typeable v, Ord k, Show k) => Steppable (IOHM.InsOrdHashMap k v) v where
  data Step (IOHM.InsOrdHashMap k v) v = InsOrdHashMapKeyStep k
    deriving stock (Eq, Ord, Show)

iohmStructuralWith ::
  (Ord k, Hashable k, Typeable k, Typeable v, Show k) =>
  (k -> ProdCons (Traced v) -> StructuralCompatFormula ()) ->
  ProdCons (Traced (IOHM.InsOrdHashMap k v)) ->
  StructuralCompatFormula ()
iohmStructuralWith f pc = do
  let ProdCons pEKeys cEKeys = S.fromList . IOHM.keys . extract <$> pc
  if pEKeys == cEKeys
    then
      for_
        pEKeys
        ( \eKey ->
            f eKey $ stepTraced (InsOrdHashMapKeyStep eKey) . fmap (IOHM.lookupDefault (error "impossible") eKey) <$> pc
        )
    else structuralIssue
{-# INLINE iohmStructuralWith #-}

runCompatFormula ::
  CompatFormula' q f r a ->
  Either (P.PathsPrefixTree q f r) a
runCompatFormula (Compose f) =
  calculate . runIdentity . runMemo 0 . unCompatM $ f
{-# INLINE runCompatFormula #-}

embedFormula :: Paths q r l -> CompatFormula' q f l a -> CompatFormula' q f r a
embedFormula bhv (Compose x) = Compose $ mapErrors (P.embed bhv) <$> x

issueAt :: Issuable l => Paths q r l -> Issue l -> CompatFormula' q AnIssue r a
issueAt xs issue = Compose $ pure $ anError $ AnItem xs $ anIssue issue
{-# INLINE issueAt #-}

anIssue :: Issuable l => Issue l -> AnIssue l
anIssue = AnIssue Forward
{-# INLINE anIssue #-}

anItem :: AnItem q AnIssue r -> CompatFormula' q AnIssue r a
anItem = Compose . pure . anError
{-# INLINE anItem #-}

invertIssueOrientation :: CompatFormula' q AnIssue r a -> CompatFormula' q AnIssue r a
invertIssueOrientation (Compose x) =
  Compose $ mapErrors invertIssueOrientationP <$> x

invertIssueOrientationP :: P.PathsPrefixTree q AnIssue r -> P.PathsPrefixTree q AnIssue r
invertIssueOrientationP = P.map (\(AnIssue ori i) -> AnIssue (toggleOrientation ori) i)

structuralIssue :: StructuralCompatFormula a
structuralIssue = Compose $ pure $ anError $ AnItem Root Proxy

anyOfAt ::
  Issuable l =>
  Paths q r l ->
  Issue l ->
  [CompatFormula' q AnIssue r a] ->
  CompatFormula' q AnIssue r a
anyOfAt _ _ [x] = x
anyOfAt xs issue fs =
  Compose $ (`eitherOf` AnItem xs (anIssue issue)) <$> traverse getCompose fs

-- | If the given formula contains any issues, add another issue on top. Otherwise succeed.
clarifyIssue ::
  AnItem q AnIssue r ->
  CompatFormula' q AnIssue r a ->
  CompatFormula' q AnIssue r a
clarifyIssue item f =
  Compose ((`eitherOf` item) <$> pure <$> getCompose f) *> f

fixpointKnot ::
  MonadState (MemoState VarRef) m =>
  KnotTier (FormulaF q f r ()) VarRef m
fixpointKnot =
  KnotTier
    { onKnotFound = modifyMemoNonce succ
    , onKnotUsed = \i -> pure $ variable i
    , tieKnot = \i x -> pure $ maxFixpoint i x
    }

memo ::
  (Typeable (l :: k), Typeable q, Typeable f, Typeable k, Typeable a) =>
  Paths q r l ->
  MemoKey ->
  (ProdCons (Traced a) -> CompatFormula' q f r ()) ->
  (ProdCons (Traced a) -> CompatFormula' q f r ())
memo bhv k f pc = Compose $ do
  formula' <-
    memoWithKnot
      fixpointKnot
      ( do
          formula <- getCompose $ f pc
          pure $ mapErrors (P.takeSubtree bhv) formula
      )
      (k, ask <$> pc)
  pure $ mapErrors (P.embed bhv) formula'

data MemoKey = SemanticMemoKey | StructuralMemoKey
  deriving stock (Eq, Ord)
