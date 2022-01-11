module Data.OpenApi.Compare.Validate.Schema.Partition
  ( partitionSchema,
    partitionRefSchema,
    selectPartition,
    runPartitionM,
    tryPartition,
    showPartition,
    intersectSchema,
    intersectRefSchema,
    IntersectionResult (..),
    runIntersectionM,
    Partition,
  )
where

import Algebra.Lattice
import Algebra.Lattice.Lifted
import Control.Applicative
import Control.Monad.Reader hiding (ask)
import qualified Control.Monad.Reader as R
import Control.Monad.State
import qualified Control.Monad.Trans.Reader as R (liftCatch)
import qualified Control.Monad.Trans.Writer as W (liftCatch)
import Control.Monad.Writer
import qualified Data.Aeson as A
import Data.Foldable
import Data.Functor.Identity
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.List (sortBy)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Memo
import Data.OpenApi.Compare.References
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema.DNF
import Data.OpenApi.Compare.Validate.Schema.JsonFormula
import Data.OpenApi.Compare.Validate.Schema.Traced
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder hiding (Format, Null)

data PartitionData
  = DByEnumValue (DNF (S.Set A.Value))
  | DByProperties (DNF (S.Set Text, S.Set Text)) -- optional, required
  deriving stock (Eq, Ord, Show)

conjPart :: PartitionData -> PartitionData -> Maybe PartitionData
conjPart (DByEnumValue xss) (DByEnumValue yss) = Just . DByEnumValue $ xss /\ yss
conjPart (DByProperties xss) (DByProperties yss) = Just . DByProperties $ xss /\ yss
conjPart _ _ = Nothing

disjPart :: PartitionData -> PartitionData -> Maybe PartitionData
disjPart (DByEnumValue xss) (DByEnumValue yss) = Just . DByEnumValue $ xss \/ yss
disjPart (DByProperties xss) (DByProperties yss) = Just . DByProperties $ xss \/ yss
disjPart _ _ = Nothing

newtype Partitions = Partitions (M.Map PartitionLocation (S.Set PartitionData))
  deriving stock (Eq, Ord, Show)

instance Lattice Partitions where
  Partitions xss /\ Partitions yss = Partitions $ M.unionWith conj xss yss
    where
      conj xs ys = S.fromList . catMaybes $ liftA2 conjPart (S.toList xs) (S.toList ys)
  Partitions xss \/ Partitions yss = Partitions $ M.intersectionWith disj xss yss
    where
      disj xs ys = S.fromList . catMaybes $ liftA2 disjPart (S.toList xs) (S.toList ys)

instance BoundedMeetSemiLattice Partitions where
  top = Partitions M.empty

-- The lattice has no bottom, but we use 'Lifted' to adjoin a free bottom element

type PartitionM = ReaderT (Traced (Definitions Schema)) (State (MemoState ()))

ignoreKnot :: KnotTier (Lifted Partitions) () PartitionM
ignoreKnot =
  KnotTier
    { onKnotFound = pure ()
    , onKnotUsed = \_ -> pure bottom
    , tieKnot = \_ -> pure
    }

singletonPart :: PartitionData -> Lifted Partitions
singletonPart = Lift . Partitions . M.singleton PHere . S.singleton

partitionSchema :: Traced Schema -> PartitionM (Lifted Partitions)
partitionSchema sch = do
  allClauses <- case tracedAllOf sch of
    Nothing -> pure []
    Just xs -> mapM partitionRefSchema xs

  anyClause <- case tracedAnyOf sch of
    Nothing -> pure top
    Just xs -> joins <$> mapM partitionRefSchema xs

  oneClause <- case tracedOneOf sch of
    Nothing -> pure top
    Just xs -> joins <$> mapM partitionRefSchema xs

  byEnumClause <- case _schemaEnum $ extract sch of
    Nothing -> pure top
    Just xs ->
      pure . singletonPart $
        DByEnumValue $ LiteralDNF (S.fromList xs)

  -- We can only partition by presence of a property if additional properties
  -- are disallowed, and the property is not optional
  let reqd = S.fromList $ _schemaRequired $ extract sch
  byPropertiesClause <- case _schemaAdditionalProperties $ extract sch of
    Just (AdditionalPropertiesAllowed False) -> do
      let props = S.fromList . IOHM.keys . _schemaProperties $ extract sch
      pure . singletonPart $
        DByProperties $ LiteralDNF (props S.\\ reqd, props `S.intersection` reqd)
    _ -> pure top

  -- We can partition on something nested in a property only if the property is
  -- required
  let reqdProps = IOHM.filterWithKey (\k _ -> k `S.member` reqd) $ tracedProperties sch
  inPropertiesClauses <- forM (IOHM.toList reqdProps) $ \(k, rs) -> do
    f <- partitionRefSchema rs
    pure $ fmap (\(Partitions m) -> Partitions $ M.mapKeysMonotonic (PInProperty k) m) f

  pure $ meets $ allClauses <> [anyClause, oneClause, byEnumClause, byPropertiesClause] <> inPropertiesClauses

partitionRefSchema :: Traced (Referenced Schema) -> PartitionM (Lifted Partitions)
partitionRefSchema x = do
  defs <- R.ask
  memoWithKnot ignoreKnot (partitionSchema $ dereference defs x) (ask x)

partitionCondition :: Condition t -> PartitionM (Lifted Partitions)
partitionCondition = \case
  Exactly x ->
    pure . singletonPart $
      DByEnumValue $ LiteralDNF (S.singleton $ untypeValue x)
  Properties props _ madd -> do
    let byProps = case madd of
          Just _ -> top
          Nothing ->
            singletonPart $
              DByProperties $
                LiteralDNF
                  ( M.keysSet $ M.filter (not . propRequired) props
                  , M.keysSet $ M.filter propRequired props
                  )
    inProps <- forM (M.toList $ M.filter propRequired props) $ \(k, prop) -> do
      f <- partitionRefSchema $ propRefSchema prop
      pure $ fmap (\(Partitions m) -> Partitions $ M.mapKeysMonotonic (PInProperty k) m) f
    pure $ byProps /\ meets inProps
  _ -> pure top

runPartitionM :: Traced (Definitions Schema) -> PartitionM a -> a
runPartitionM defs = runIdentity . runMemo () . (`runReaderT` defs)

partitionJsonFormulas ::
  ProdCons (Traced (Definitions Schema)) ->
  ProdCons (JsonFormula t) ->
  Lifted Partitions
partitionJsonFormulas defs pc = producer pcPart \/ consumer pcPart
  where
    pcPart = partitionFormula <$> defs <*> pc
    partitionFormula def (JsonFormula xss) = runPartitionM def $ forDNF partitionCondition xss

selectPartition :: Lifted Partitions -> Maybe (PartitionLocation, S.Set PartitionChoice)
selectPartition Bottom = Nothing
selectPartition (Lift (Partitions m)) =
  go [(loc, part) | (loc, parts) <- sortBy (comparing $ locLength . fst) $ M.toList m, part <- S.toList parts]
  where
    locLength :: PartitionLocation -> Int
    locLength = walk 0
      where
        walk !n PHere = n
        walk !n (PInProperty _ l) = walk (n + 1) l
    go [] = Nothing
    -- Skip partitioning by property for now
    go ((_, DByProperties _) : ps) = go ps
    -- Don't partition by enum value at the root (this reports removed enum values as contradictions in their respective partitions)
    go ((PHere, DByEnumValue _) : ps) = go ps
    go ((loc, DByEnumValue (DNF xss)) : ps)
      -- Check that no disjunction branches are unresticted
      | Just enums <- traverse (\(Disjunct xs) -> fmap (foldr1 S.intersection) . NE.nonEmpty . S.toList $ xs) . S.toList $ xss =
        -- TODO: improve
        Just (loc, S.map (CByEnumValue . S.singleton) $ S.unions enums)
      | otherwise = go ps

-- This essentially has 3 cases:
-- Nothing -- we have produced a bottom schema
-- Just (False, _) -- there's been no change to the schema
-- Just (True, x) -- x is a new schema
type IntersectionM = ReaderT (Traced (Definitions Schema)) (WriterT Any Maybe)

mBottom :: IntersectionM a
mBottom = lift . lift $ Nothing

catchBottom :: IntersectionM a -> IntersectionM a -> IntersectionM a
catchBottom act handler = R.liftCatch (W.liftCatch (\a h -> a <|> h ())) act (\_ -> handler)

mChange :: IntersectionM ()
mChange = tell $ Any True

data IntersectionResult a = Disjoint | Same a | New a
  deriving stock (Eq, Ord, Show)

runIntersectionM :: Traced (Definitions Schema) -> IntersectionM a -> IntersectionResult a
runIntersectionM defs act = case runWriterT $ runReaderT act defs of
  Nothing -> Disjoint
  Just (x, Any False) -> Same x
  Just (x, Any True) -> New x

intersectSchema ::
  PartitionLocation ->
  PartitionChoice ->
  Traced Schema ->
  IntersectionM Schema
intersectSchema loc part sch = do
  allOf' <- forM (tracedAllOf sch) $ \rss ->
    -- Assuming i ranges over a nonempty set (checked in processSchema)
    -- (⋂_i A[i]) ∩ X = ⋂_i (A[i] ∩ X)
    -- If any intersections are empty, the result is empty. If any intersections are a change, the result is a change.
    traverse (intersectRefSchema loc part) rss
  anyOf' <- forM (tracedAnyOf sch) $ \rss -> do
    -- (⋃_i A[i]) ∩ X = ⋃_i (A[i] ∩ X)
    -- Collect only the nonempty A[i] ∩ X, unless there are none, in which case the result is empty.
    -- If any schema is empty, we remove it from the list which constitutes a change.
    mSchemas <- forM rss $ \rs -> catchBottom (Just <$> intersectRefSchema loc part rs) (mChange >> pure Nothing)
    case catMaybes mSchemas of
      [] -> mBottom
      schs -> pure schs
  oneOf' <- forM (tracedOneOf sch) $ \rss -> do
    -- Same as anyOf'. By intersecting we're only making them more disjoint if anything.
    mSchemas <- forM rss $ \rs -> catchBottom (Just <$> intersectRefSchema loc part rs) (mChange >> pure Nothing)
    case catMaybes mSchemas of
      [] -> mBottom
      schs -> pure schs
  let sch' = (extract sch) {_schemaAllOf = allOf', _schemaAnyOf = anyOf', _schemaOneOf = oneOf'}
  -- Now the local changes:
  case loc of
    PInProperty k loc' -> case IOHM.lookup k $ tracedProperties sch of
      Nothing -> error $ "Partitioning via absent property: " <> T.unpack k
      Just prop -> do
        prop' <- intersectRefSchema loc' part prop
        pure $ sch' {_schemaProperties = IOHM.adjust (const prop') k $ _schemaProperties sch'}
    PHere -> case part of
      CByEnumValue vals -> do
        enum' <- case _schemaEnum sch' of
          Nothing -> do
            mChange
            pure $ S.toList vals
          Just xs -> do
            when (any (`S.notMember` vals) xs) mChange
            case filter (`S.member` vals) xs of
              [] -> mBottom
              xs' -> pure xs'
        pure $ sch' {_schemaEnum = Just enum'}
      CByProperties {} -> error "CByProperties not implemented"

intersectRefSchema ::
  PartitionLocation ->
  PartitionChoice ->
  Traced (Referenced Schema) ->
  IntersectionM (Referenced Schema)
intersectRefSchema loc part rs = do
  defs <- R.ask
  Inline <$> intersectSchema loc part (dereference defs rs)

intersectCondition :: Traced (Definitions Schema) -> PartitionLocation -> PartitionChoice -> Condition t -> DNF (Condition t)
intersectCondition _defs PHere (CByEnumValue values) cond@(Exactly x) =
  if untypeValue x `S.member` values then LiteralDNF cond else bottom
intersectCondition defs (PInProperty k loc) part cond@(Properties props add madd) = case M.lookup k props of
  Nothing -> LiteralDNF cond -- shouldn't happen
  Just prop -> case runIntersectionM defs $ intersectRefSchema loc part $ propRefSchema prop of
    New rs' ->
      let trs' = traced (ask (propRefSchema prop) >>> step (Partitioned (loc, part))) rs'
       in LiteralDNF $ Properties (M.insert k prop {propRefSchema = trs'} props) add madd
    Same _ -> LiteralDNF cond
    Disjoint -> bottom
intersectCondition _defs _loc _part cond = LiteralDNF cond

intersectFormula :: Traced (Definitions Schema) -> PartitionLocation -> PartitionChoice -> JsonFormula t -> JsonFormula t
intersectFormula defs loc part = JsonFormula . foldDNF (intersectCondition defs loc part) . getJsonFormula

tryPartition :: ProdCons (Traced (Definitions Schema)) -> ProdCons (JsonFormula t) -> [(Maybe Partition, ProdCons (JsonFormula t))]
tryPartition defs pc = case selectPartition $ partitionJsonFormulas defs pc of
  Nothing -> [(Nothing, pc)]
  Just (loc, parts) -> [(Just (loc, part), intersectFormula <$> defs <*> pure loc <*> pure part <*> pc) | part <- S.toList parts]

showPartition :: Partition -> Inlines
showPartition = \case
  (partition, CByEnumValue (S.toList -> [v])) ->
    renderPartitionLocation partition <> " is " <> showJSONValueInline v
  (partition, CByEnumValue (S.toList -> vs)) ->
    renderPartitionLocation partition <> " has values: "
      <> (fold . L.intersperse ", " . fmap showJSONValueInline $ vs)
  (partition, CByProperties (S.toList -> incl) (S.toList -> [])) ->
    renderPartitionLocation partition <> " contains the properties: " <> listCodes incl
  (partition, CByProperties (S.toList -> []) (S.toList -> excl)) ->
    renderPartitionLocation partition <> " does not contain the properties: " <> listCodes excl
  (partition, CByProperties (S.toList -> incl) (S.toList -> excl)) ->
    renderPartitionLocation partition
      <> " contains the properties "
      <> listCodes incl
      <> " and does not contain the properties "
      <> listCodes excl
  where
    listCodes :: [Text] -> Inlines
    listCodes = fold . L.intersperse ", " . fmap code
    renderPartitionLocation :: PartitionLocation -> Inlines
    renderPartitionLocation p = code $ "$" <> renderPartitionLocation' p
      where
        renderPartitionLocation' :: PartitionLocation -> Text
        renderPartitionLocation' PHere = mempty
        renderPartitionLocation' (PInProperty prop rest) = "." <> prop <> renderPartitionLocation' rest
