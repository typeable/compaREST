{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.Schema
  (
  )
where

import Control.Monad.Writer
import qualified Data.Aeson as A
import Data.Coerce
import Data.Foldable (for_, toList)
import Data.Functor
import Data.HList
import Data.List (genericIndex, genericLength, group)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Paths
import qualified Data.OpenApi.Compare.PathsPrefixTree as P
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema.DNF
import Data.OpenApi.Compare.Validate.Schema.Issues
import Data.OpenApi.Compare.Validate.Schema.JsonFormula
import Data.OpenApi.Compare.Validate.Schema.Partition
import Data.OpenApi.Compare.Validate.Schema.Process
import Data.OpenApi.Compare.Validate.Schema.Traced
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.Ord
import Data.Ratio
import Data.Semigroup
import qualified Data.Set as S
import Data.Text (Text)

checkFormulas ::
  (ReassembleHList xs (CheckEnv (Referenced Schema))) =>
  HList xs ->
  Behavior 'SchemaLevel ->
  ProdCons (Trace Schema) ->
  ProdCons (Traced (Definitions Schema)) ->
  ProdCons (ForeachType JsonFormula, P.PathsPrefixTree Behave AnIssue 'SchemaLevel) ->
  SemanticCompatFormula ()
checkFormulas env beh trs defs (ProdCons (fp, ep) (fc, ec)) =
  case P.toList ep ++ P.toList ec of
    issues@(_ : _) -> for_ issues $ embedFormula beh . anItem
    [] -> do
      -- We have the following isomorphisms:
      --   (A ⊂ X ∩ Y) = (A ⊂ X) /\ (A ⊂ Y)
      --   (A ⊂ ⊤) = 1
      --   (X ∪ Y ⊂ B) = (X ⊂ B) /\ (Y ⊂ B)
      --   (∅ ⊂ B) = 1
      -- The remaining cases are, notably, not isomorphisms:
      --   1) (A ⊂ X ∪ Y) <= (A ⊂ X) \/ (A ⊂ Y)
      --   2) (A ⊂ ∅) <= 0
      --   3) (X ∩ Y ⊂ B) <= (X ⊂ B) \/ (Y ⊂ B)
      --   4) (⊤ ⊂ B) <= 0
      -- Therefore we have the implications with (∃ and ∀ being the N-ary
      -- versions of \/ and /\ respectively):
      --   (⋃_i ⋂_j A[i,j]) ⊂ (⋃_k ⋂_l B[k,l])
      --   <= ∃k ∀l ∀i, (⋂_j A[i,j]) ⊂ B[k,l]
      --   = ∀i ∃k ∀l, (⋂_j A[i,j]) ⊂ B[k,l]
      -- with the caveat that the the set over which k ranges is nonempty.
      -- (because 2) is not an isomorphism), and that this is a sufficient
      -- but not necessary condition (because 1) is not an isomorphism).
      -- Our disjunction loses information, so it makes sense to nest it as
      -- deeply as possible, hence we choose the latter representation.
      --
      -- We delegate the verification of (⋂_j A[j]) ⊂ B to a separate heuristic
      -- function, with the understanding that ∃j, A[j] ⊂ B is a sufficient,
      -- but not necessary condition (because of 3) and 4)).
      --
      -- If k ranges over an empty set, we have the isomorphism:
      --   (⋃_i ⋂_j A[i,j]) ⊂ ∅ = ∀i, (⋂_j A[i,j]) ⊂ ∅
      -- where we again delegate (⋂_j A[j]) ⊂ ∅ to a heuristic, though here the
      -- shortcut of ∃j, A[j] ⊂ ∅ hardly helps.
      --
      -- Disjunctions tend to erase informative error messages, so we may want
      -- to avoid them. This can be formally done as follows: if we can
      -- partition the universal set into a disjoint union of some parts:
      --   ⊤ = ⊔_α P[α]
      -- such that the conjuncts in our disjunctive normal form are subordinate
      -- to the partition:
      --   ∀i ∃α, (⋂_j A[i,j]) ⊂ P[α]
      --   ∀k ∃α, (⋂_l B[k,l]) ⊂ P[α]
      -- then we can partition the sets over which i and k range into partitions
      -- I[α] and K[α], and then in each "bucket" verify the inclusion in the
      -- aforementioned way:
      --   ∀α, (⋃_i∈I[α] ⋂_j A[i,j]) ⊂ (⋃_k∈K[α] ⋂_l B[k,l])
      --   = ∀α ∀i∈I[α] ∃k∈K[α] ∀l, (⋂_j A[i,j]) ⊂ B[k,l]
      -- We already somewhat do this by partitioning JSON into types, but we can
      -- additionally partition e.g. "enum" fields or existence of particular
      -- properties. This works especially well if we manage to ensure K[α] are
      -- 1-element sets.
      --
      -- Since the set:
      --   (⋃_i∈I[α] ⋂_j A[i,j]) = (⋃_i ⋂_j A[i,j]) ∩ P[α]
      -- does not actually appear in the source schema, we need to construct it
      -- ourselves and come up with a name for it.
      let typesRestricted = not (anyBottomTypes fp) && anyBottomTypes fc
      -- Specifically handle the case when a schema's type has been
      -- restricted from "all" to specific types: if all types were allowed
      -- in the producer and not all types are allowed in the consumer, it's
      -- usually easier to say what's left than what's removed
      when typesRestricted $ issueAt beh $ TypesRestricted $ nonBottomTypes fc
      forType_ $ \tyName ty -> do
        let beh' = beh >>> step (OfType tyName)
        case (getJsonFormula $ ty fp, getJsonFormula $ ty fc) of
          (DNF pss, BottomDNF) -> unless typesRestricted $ do
            -- don't repeat the TypesRestricted issue
            for_ pss $ \(Disjunct ps) -> checkContradiction beh' Nothing ps
          (DNF pss, SingleDisjunct (Disjunct cs)) -> for_ pss $ \(Disjunct ps) -> do
            for_ cs $ checkImplication env beh' trs ps -- avoid disjunction if there's only one conjunct
          (TopDNF, DNF css) ->
            -- producer is "open" (allows any value), but consumer has restrictions.
            -- In this case we want to show which restrictions were added. (instead
            -- of showing an empty list restrictions that couldn't be satisfied.)
            for_ css $ \(Disjunct cs) -> for_ cs $ checkImplication env beh' trs S.empty
          (pss', css') -> for_ (tryPartition defs $ ProdCons (JsonFormula pss') (JsonFormula css')) $ \case
            (mPart, ProdCons pf cf) -> do
              let beh'' = foldr ((<<<) . step . InPartition) beh' mPart
              case (getJsonFormula pf, getJsonFormula cf) of
                (DNF pss, BottomDNF) -> for_ pss $ \(Disjunct ps) -> checkContradiction beh' mPart ps
                (DNF pss, SingleDisjunct (Disjunct cs)) -> for_ pss $ \(Disjunct ps) -> do
                  for_ cs $ checkImplication env beh'' trs ps
                -- unlucky:
                (DNF pss, DNF css) -> for_ pss $ \(Disjunct ps) -> do
                  anyOfAt
                    beh'
                    (issueFromDisjunct Nothing ps)
                    [for_ cs $ checkImplication env beh' trs ps | Disjunct cs <- S.toList css]
      pure ()
  where
    anyBottomTypes f = getAny $
      foldType $ \_ ty -> case getJsonFormula $ ty f of
        BottomDNF -> Any True
        _ -> mempty
    nonBottomTypes f = foldType $ \tyName ty -> case getJsonFormula $ ty f of
      BottomDNF -> mempty
      _ -> [tyName]
    issueFromDisjunct :: Typeable t => Maybe Partition -> S.Set (Condition t) -> Issue 'TypedSchemaLevel
    issueFromDisjunct _ ps
      | Just e <- findExactly ps
        , all (satisfiesTyped e) ps =
        EnumDoesntSatisfy $ untypeValue e -- what does this look like when partitioned?
    issueFromDisjunct mPart ps = NoMatchingCondition mPart $ SomeCondition <$> S.toList ps

checkContradiction ::
  Behavior 'TypedSchemaLevel ->
  Maybe Partition ->
  S.Set (Condition t) ->
  SemanticCompatFormula ()
checkContradiction beh mPart _ = issueAt beh $ maybe TypeBecomesEmpty PartitionBecomesEmpty mPart -- TODO #70

checkImplication ::
  (ReassembleHList xs (CheckEnv (Referenced Schema))) =>
  HList xs ->
  Behavior 'TypedSchemaLevel ->
  ProdCons (Trace Schema) -> -- the traces of the root schemas used in this comparison
  S.Set (Condition t) ->
  Condition t ->
  SemanticCompatFormula ()
checkImplication env beh trs prods cons = case findExactly prods of
  Just e
    | all (satisfiesTyped e) prods ->
      if satisfiesTyped e cons
        then pure ()
        else issueAt beh (EnumDoesntSatisfy $ untypeValue e)
    | otherwise -> pure () -- vacuously true
  Nothing -> case cons of
    -- the above code didn't catch it, so there's no Exactly condition on the lhs
    Exactly e -> issueAt beh (NoMatchingEnum $ untypeValue e)
    Maximum m -> foldCheck min m NoMatchingMaximum MatchingMaximumWeak $ \case
      Maximum m' -> Just m'
      _ -> Nothing
    Minimum m -> foldCheck max m (NoMatchingMinimum . coerce) (MatchingMinimumWeak . coerce) $ \case
      Minimum m' -> Just m'
      _ -> Nothing
    MultipleOf m -> foldCheck lcmScientific m NoMatchingMultipleOf MatchingMultipleOfWeak $ \case
      MultipleOf m' -> Just m'
      _ -> Nothing
    NumberFormat f -> case flip any prods $ \case
      NumberFormat f' -> f == f'
      _ -> False of
      True -> pure ()
      False -> issueAt beh (NoMatchingFormat f)
    MaxLength m -> foldCheck min m NoMatchingMaxLength MatchingMaxLengthWeak $ \case
      MaxLength m' -> Just m'
      _ -> Nothing
    MinLength m -> foldCheck max m NoMatchingMinLength MatchingMinLengthWeak $ \case
      MinLength m' -> Just m'
      _ -> Nothing
    Pattern p -> case flip any prods $ \case
      Pattern p' -> p == p'
      _ -> False of
      True -> pure ()
      False -> issueAt beh (NoMatchingPattern p) -- TODO: regex comparison #32
    StringFormat f -> case flip any prods $ \case
      StringFormat f' -> f == f'
      _ -> False of
      True -> pure ()
      False -> issueAt beh (NoMatchingFormat f)
    Items _ cons' -> case foldSome (<>) prods $ \case
      Items _ rs -> Just (Just (rs NE.:| []), mempty)
      TupleItems (map snd -> fs) -> Just (mempty, Just (fs NE.:| []))
      _ -> Nothing of
      Just (mItems, Just pfs)
        | not $ allSame (length <$> pfs) -> pure () -- vacuously
        | let plen = genericLength (NE.head pfs) ->
          clarifyIssue (AnItem beh (anIssue TupleToArray)) $
            for_ [0 .. plen - 1] $ \i -> do
              let prod' = tracedConjunct $ case mItems of
                    Just prods' -> ((`genericIndex` i) <$> pfs) <> prods'
                    Nothing -> (`genericIndex` i) <$> pfs
              checkCompatibility (beh >>> step (InItem i)) env $ ProdCons prod' cons'
      Just (Just prods', Nothing) -> do
        let prod' = tracedConjunct prods'
        checkCompatibility (beh >>> step InItems) env $ ProdCons prod' cons'
      _ -> clarifyIssue (AnItem beh (anIssue NoMatchingItems)) $ do
        checkCompatibility (beh >>> step InItems) env $ ProdCons prodTopSchema cons'
    TupleItems (map snd -> fs) -> case foldSome (<>) prods $ \case
      TupleItems (map snd -> fs') -> Just (Just $ fs' NE.:| [], Just . Max $ genericLength fs', Just . Min $ genericLength fs', mempty)
      MinItems m' -> Just (mempty, Just . Max $ m', mempty, mempty)
      MaxItems m' -> Just (mempty, mempty, Just . Min $ m', mempty)
      Items _ rs -> Just (mempty, mempty, mempty, Just (rs NE.:| []))
      _ -> Nothing of
      -- if the length constraints in the producer are contradictory:
      Just (_, Just (Max lowest), Just (Min highest), _) | lowest > highest -> pure ()
      -- We have an explicit tuple items clause...
      Just (Just pfs, Just (Max plen), _, _)
        | plen /= genericLength fs -> -- ...of wrong length
          issueAt beh (TupleItemsLengthChanged ProdCons {producer = plen, consumer = genericLength fs})
        | otherwise ->
          for_ [0 .. plen - 1] $ \i -> do
            checkCompatibility (beh >>> step (InItem i)) env $ ProdCons (tracedConjunct $ (`genericIndex` i) <$> pfs) (fs `genericIndex` i)
      -- We have a fixed length array in the producer...
      Just (Nothing, Just (Max plen), Just (Min plen'), mProd)
        | plen == plen' ->
          clarifyIssue (AnItem beh (anIssue ArrayToTuple)) $ case mProd of
            _
              | plen /= genericLength fs -> -- ...of wrong length
                issueAt beh (TupleItemsLengthChanged ProdCons {producer = plen, consumer = genericLength fs})
            Just rs -> for_ [0 .. plen - 1] $ \i -> do
              checkCompatibility (beh >>> step (InItem i)) env $ ProdCons (tracedConjunct rs) (fs `genericIndex` i)
            -- ...and no "items" schema
            Nothing -> clarifyIssue (AnItem beh (anIssue NoMatchingTupleItems)) $ do
              for_ [0 .. plen - 1] $ \i -> do
                checkCompatibility (beh >>> step (InItem i)) env $ ProdCons prodTopSchema (fs `genericIndex` i)
      _ -> issueAt beh NoMatchingTupleItems
    MaxItems m -> foldCheck min m NoMatchingMaxItems MatchingMaxItemsWeak $ \case
      MaxItems m' -> Just m'
      TupleItems fs -> Just $ toInteger $ length fs
      _ -> Nothing
    MinItems m -> foldCheck max m NoMatchingMinItems MatchingMinItemsWeak $ \case
      MinItems m' -> Just m'
      TupleItems fs -> Just $ toInteger $ length fs
      _ -> Nothing
    UniqueItems -> case flip any prods $ \case
      UniqueItems -> True
      MaxItems 1 -> True
      TupleItems fs | length fs == 1 -> True
      _ -> False of
      True -> pure ()
      False -> issueAt beh NoMatchingUniqueItems
    Properties props _ madd -> case foldSome (<>) prods $ \case
      Properties props' _ madd' -> Just $ (props', madd') NE.:| []
      _ -> Nothing of
      Just pm ->
        anyOfAt beh NoMatchingProperties $ -- TODO: could first "concat" the lists
          NE.toList pm <&> \(props', madd') -> do
            for_ (S.fromList $ M.keys props <> M.keys props') $ \k -> do
              let beh' = beh >>> step (InProperty k)
                  go sch sch' = checkCompatibility beh' env (ProdCons sch sch')
              case (maybe False propRequired $ M.lookup k props', maybe False propRequired $ M.lookup k props) of
                -- producer does not require field, but consumer does (can fail)
                (False, True) -> issueAt beh' PropertyNowRequired
                _ -> pure ()
              case (M.lookup k props', madd', M.lookup k props, madd) of
                -- (producer, additional producer, consumer, additional consumer)
                (Nothing, Nothing, _, _) -> pure () -- vacuously: the producer asserts that this field cannot exist,
                -- and the consumer either doesn't require it, or it does and we've already raised an error about it.
                (_, _, Nothing, Nothing) -> issueAt beh' UnexpectedProperty
                (Just p', _, Just p, _) -> go (propRefSchema p') (propRefSchema p)
                (Nothing, Just add', Just p, _) ->
                  clarifyIssue (AnItem beh' (anIssue AdditionalToProperty)) $
                    go add' (propRefSchema p)
                (Just p', _, Nothing, Just add) ->
                  clarifyIssue (AnItem beh' (anIssue PropertyToAdditional)) $
                    go (propRefSchema p') add
                (Nothing, Just _, Nothing, Just _) -> pure ()
              pure ()
            case (madd', madd) of
              (Nothing, _) -> pure () -- vacuously
              (_, Nothing) -> issueAt beh NoAdditionalProperties
              (Just add', Just add) -> checkCompatibility (beh >>> step InAdditionalProperty) env (ProdCons add' add)
            pure ()
      Nothing -> issueAt beh NoMatchingProperties
    MaxProperties m -> foldCheck min m NoMatchingMaxProperties MatchingMaxPropertiesWeak $ \case
      MaxProperties m' -> Just m'
      _ -> Nothing
    MinProperties m -> foldCheck max m NoMatchingMinProperties MatchingMinPropertiesWeak $ \case
      MinProperties m' -> Just m'
      _ -> Nothing
  where
    lcmScientific (toRational -> a) (toRational -> b) =
      fromRational $ lcm (numerator a) (numerator b) % gcd (denominator a) (denominator b)

    foldCheck ::
      Eq a =>
      (a -> a -> a) ->
      a ->
      (a -> Issue 'TypedSchemaLevel) ->
      (ProdCons a -> Issue 'TypedSchemaLevel) ->
      (forall t. Condition t -> Maybe a) ->
      SemanticCompatFormula ()
    foldCheck f m missing weak extr = case foldSome f prods extr of
      Just m'
        | f m' m == m' -> pure ()
        | otherwise -> issueAt beh (weak ProdCons {producer = m', consumer = m})
      Nothing -> issueAt beh (missing m)

    prodTopSchema = traced (producer trs >>> step ImplicitTopSchema) $ Inline mempty

allSame :: (Foldable f, Eq a) => f a -> Bool
allSame xs = case group (toList xs) of
  [] -> True
  [_] -> True
  _ -> False

foldSome :: (b -> b -> b) -> S.Set a -> (a -> Maybe b) -> Maybe b
foldSome combine xs extr =
  fmap (foldr1 combine) . NE.nonEmpty . mapMaybe extr . S.toList $ xs

findExactly :: S.Set (Condition t) -> Maybe (TypedValue t)
findExactly xs = foldSome const xs $ \case
  Exactly x -> Just x
  _ -> Nothing

instance Subtree Schema where
  type SubtreeLevel Schema = 'SchemaLevel
  type CheckEnv Schema = '[ProdCons (Traced (Definitions Schema))]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _schemaRequired <$> pc
    structuralEq $ fmap _schemaNullable <$> pc
    structuralMaybeWith (structuralList env) $ tracedAllOf <$> pc
    structuralMaybeWith (structuralList env) $ tracedOneOf <$> pc
    structuralMaybe env $ sequence . stepTraced NotStep . fmap _schemaNot <$> pc
    structuralMaybeWith (structuralList env) $ tracedAnyOf <$> pc
    iohmStructural env $ stepTraced PropertiesStep . fmap _schemaProperties <$> pc
    structuralMaybeWith structuralAdditionalProperties $ tracedAdditionalProperties <$> pc
    structuralMaybeWith structuralDiscriminator $ tracedDiscriminator <$> pc
    structuralEq $ fmap _schemaReadOnly <$> pc
    structuralEq $ fmap _schemaWriteOnly <$> pc
    structuralEq $ fmap _schemaXml <$> pc
    structuralEq $ fmap _schemaMaxProperties <$> pc
    structuralEq $ fmap _schemaMinProperties <$> pc
    structuralEq $ fmap _schemaDefault <$> pc
    structuralEq $ fmap _schemaType <$> pc
    structuralEq $ fmap _schemaFormat <$> pc
    structuralMaybeWith structuralItems $ tracedItems <$> pc
    structuralEq $ fmap _schemaMaximum <$> pc
    structuralEq $ fmap _schemaExclusiveMaximum <$> pc
    structuralEq $ fmap _schemaMinimum <$> pc
    structuralEq $ fmap _schemaExclusiveMinimum <$> pc
    structuralEq $ fmap _schemaMaxLength <$> pc
    structuralEq $ fmap _schemaMinLength <$> pc
    structuralEq $ fmap _schemaPattern <$> pc
    structuralEq $ fmap _schemaMaxItems <$> pc
    structuralEq $ fmap _schemaMinItems <$> pc
    structuralEq $ fmap _schemaUniqueItems <$> pc
    structuralEq $ fmap _schemaEnum <$> pc
    structuralEq $ fmap _schemaMultipleOf <$> pc
    pure ()
    where
      structuralAdditionalProperties
        (ProdCons (Left x) (Left y)) = unless (x == y) structuralIssue
      structuralAdditionalProperties
        (ProdCons (Right x) (Right y)) =
          checkSubstructure env $ ProdCons x y
      structuralAdditionalProperties _ = structuralIssue
      structuralDiscriminator pc' = do
        structuralEq $ fmap _discriminatorPropertyName <$> pc'
        iohmStructural env $
          stepTraced DiscriminatorMapping . fmap (fmap parseDiscriminatorValue . _discriminatorMapping) <$> pc'
        pure ()
      structuralItems (ProdCons (Left a) (Left b)) =
        checkSubstructure env $ ProdCons a b
      structuralItems (ProdCons (Right a) (Right b)) =
        structuralList env $ ProdCons a b
      structuralItems _ = structuralIssue
  checkSemanticCompatibility env beh schs = do
    let defs = getH env
    checkFormulas env beh (ask <$> schs) defs $ schemaToFormula <$> defs <*> schs

parseDiscriminatorValue :: Text -> Referenced Schema
parseDiscriminatorValue v = case A.fromJSON @(Referenced Schema) $ A.object ["$ref" A..= v] of
  A.Success x -> x
  A.Error _ -> Ref $ Reference v
