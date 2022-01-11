module Data.OpenApi.Compare.Validate.Schema.Process
  ( schemaToFormula,
  )
where

import Algebra.Lattice
import Control.Monad.Reader hiding (ask)
import qualified Control.Monad.Reader as R
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Aeson as A
import Data.Functor.Identity
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map as M
import Data.Maybe
import Data.OpenApi hiding (get)
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Memo
import Data.OpenApi.Compare.Paths
import qualified Data.OpenApi.Compare.PathsPrefixTree as P
import Data.OpenApi.Compare.References
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema.DNF
import Data.OpenApi.Compare.Validate.Schema.Issues
import Data.OpenApi.Compare.Validate.Schema.JsonFormula
import Data.OpenApi.Compare.Validate.Schema.Partition
import Data.OpenApi.Compare.Validate.Schema.Traced
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.Ord
import qualified Data.Set as S

-- | A fake writer monad that doesn't actually record anything and allows lazy recursion.
newtype Silent w a = Silent {runSilent :: a}
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance Monoid w => MonadWriter w (Silent w) where
  tell _ = Silent ()
  listen (Silent x) = Silent (x, mempty)
  pass (Silent (x, _)) = Silent x

instance MonadState (MemoState ()) (Silent w) where
  get = Silent $ runIdentity $ runMemo () get
  put _ = pure ()

type ProcessM = StateT (MemoState ()) (ReaderT (Traced (Definitions Schema)) (Writer (P.PathsPrefixTree Behave AnIssue 'SchemaLevel)))

type SilentM = ReaderT (Traced (Definitions Schema)) (Silent (P.PathsPrefixTree Behave AnIssue 'SchemaLevel))

-- Either SilentM or ProcessM
type MonadProcess m =
  ( MonadReader (Traced (Definitions Schema)) m
  , MonadWriter (P.PathsPrefixTree Behave AnIssue 'SchemaLevel) m
  , MonadState (MemoState ()) m
  )

warn :: MonadProcess m => Issue 'SchemaLevel -> m ()
warn issue = tell $ P.singleton $ AnItem Root $ anIssue issue

-- | Ignore warnings but allow a recursive loop that lazily computes a recursive 'Condition'.
silently :: MonadProcess m => SilentM a -> m a
silently m = do
  defs <- R.ask
  pure . runSilent $ runReaderT m defs

warnKnot :: MonadProcess m => KnotTier (ForeachType JsonFormula) () m
warnKnot =
  KnotTier
    { onKnotFound = warn UnguardedRecursion
    , onKnotUsed = \_ -> pure bottom
    , tieKnot = \_ -> pure
    }

processRefSchema ::
  MonadProcess m =>
  Traced (Referenced Schema) ->
  m (ForeachType JsonFormula)
processRefSchema x = do
  defs <- R.ask
  memoWithKnot warnKnot (processSchema $ dereference defs x) (ask x)

-- | Turn a schema into a tuple of 'JsonFormula's that describes the condition
-- for every possible type of a JSON value. The conditions are independent, and
-- are thus checked independently.
processSchema ::
  MonadProcess m =>
  Traced Schema ->
  m (ForeachType JsonFormula)
processSchema sch@(extract -> Schema {..}) = do
  let singletonFormula :: Condition t -> JsonFormula t
      singletonFormula = JsonFormula . LiteralDNF

  allClauses <- case tracedAllOf sch of
    Nothing -> pure []
    Just [] -> [] <$ warn (InvalidSchema "no items in allOf")
    Just xs -> mapM processRefSchema xs

  anyClause <- case tracedAnyOf sch of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in anyOf")
    Just xs -> joins <$> mapM processRefSchema xs

  oneClause <- case tracedOneOf sch of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in oneOf")
    Just xs -> do
      checkOneOfDisjoint xs >>= \case
        True -> pure ()
        False -> warn OneOfNotDisjoint
      joins <$> mapM processRefSchema xs

  case _schemaNot of
    Nothing -> pure ()
    Just _ -> warn (NotSupported "not clause is unsupported")

  let typeClause = case _schemaType of
        Nothing -> top
        Just OpenApiNull ->
          bottom
            { forNull = top
            }
        Just OpenApiBoolean ->
          bottom
            { forBoolean = top
            }
        Just OpenApiNumber ->
          bottom
            { forNumber = top
            }
        Just OpenApiInteger ->
          bottom
            { forNumber = singletonFormula $ MultipleOf 1
            }
        Just OpenApiString ->
          bottom
            { forString = top
            }
        Just OpenApiArray ->
          bottom
            { forArray = top
            }
        Just OpenApiObject ->
          bottom
            { forObject = top
            }

  let valueEnum A.Null =
        bottom
          { forNull = singletonFormula $ Exactly TNull
          }
      valueEnum (A.Bool b) =
        bottom
          { forBoolean = singletonFormula $ Exactly $ TBool b
          }
      valueEnum (A.Number n) =
        bottom
          { forNumber = singletonFormula $ Exactly $ TNumber n
          }
      valueEnum (A.String s) =
        bottom
          { forString = singletonFormula $ Exactly $ TString s
          }
      valueEnum (A.Array a) =
        bottom
          { forArray = singletonFormula $ Exactly $ TArray a
          }
      valueEnum (A.Object o) =
        bottom
          { forObject = singletonFormula $ Exactly $ TObject o
          }
  enumClause <- case _schemaEnum of
    Nothing -> pure top
    Just [] -> bottom <$ warn (InvalidSchema "no items in enum")
    Just xs -> pure $ joins (valueEnum <$> xs)

  let maximumClause = case _schemaMaximum of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $
                Maximum $
                  case _schemaExclusiveMaximum of
                    Just True -> Exclusive n
                    _ -> Inclusive n
            }

      minimumClause = case _schemaMinimum of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $
                Minimum $
                  Down $
                    case _schemaExclusiveMinimum of
                      Just True -> Exclusive $ Down n
                      _ -> Inclusive $ Down n
            }

      multipleOfClause = case _schemaMultipleOf of
        Nothing -> top
        Just n ->
          top
            { forNumber = singletonFormula $ MultipleOf n
            }

  formatClause <- case _schemaFormat of
    Nothing -> pure top
    Just f
      | f `elem` ["int32", "int64", "float", "double"] ->
        pure
          top
            { forNumber = singletonFormula $ NumberFormat f
            }
    Just f
      | f `elem` ["byte", "binary", "date", "date-time", "password", "uuid"] ->
        pure
          top
            { forString = singletonFormula $ StringFormat f
            }
    Just f -> top <$ warn (NotSupported $ "Unknown format: " <> f)

  let maxLengthClause = case _schemaMaxLength of
        Nothing -> top
        Just n ->
          top
            { forString = singletonFormula $ MaxLength n
            }

      minLengthClause = case _schemaMinLength of
        Nothing -> top
        Just n ->
          top
            { forString = singletonFormula $ MinLength n
            }

      patternClause = case _schemaPattern of
        Nothing -> top
        Just p ->
          top
            { forString = singletonFormula $ Pattern p
            }

  itemsClause <- case tracedItems sch of
    Nothing -> pure top
    Just (Left rs) -> do
      f <- silently $ processRefSchema rs
      pure top {forArray = singletonFormula $ Items f rs}
    Just (Right rss) -> do
      fsrs <- forM rss $ \rs -> do
        f <- silently $ processRefSchema rs
        pure (f, rs)
      pure top {forArray = singletonFormula $ TupleItems fsrs}

  let maxItemsClause = case _schemaMaxItems of
        Nothing -> top
        Just n ->
          top
            { forArray = singletonFormula $ MaxItems n
            }

      minItemsClause = case _schemaMinItems of
        Nothing -> top
        Just n ->
          top
            { forArray = singletonFormula $ MinItems n
            }

      uniqueItemsClause = case _schemaUniqueItems of
        Just True ->
          top
            { forArray = singletonFormula UniqueItems
            }
        _ -> top

  (addProps, addPropSchema) <- case tracedAdditionalProperties sch of
    Just (Right rs) -> (,Just rs) <$> silently (processRefSchema rs)
    Just (Left False) -> pure (bottom, Nothing)
    _ -> pure (top, Just $ traced (ask sch `Snoc` AdditionalPropertiesStep) $ Inline mempty)
  propList <- forM (S.toList . S.fromList $ IOHM.keys _schemaProperties <> _schemaRequired) $ \k -> do
    (f, psch) <- case IOHM.lookup k $ tracedProperties sch of
      Just rs -> (,rs) <$> silently (processRefSchema rs)
      Nothing ->
        let fakeSchema = traced (ask sch `Snoc` AdditionalPropertiesStep) $ Inline mempty
         in -- The mempty here is incorrect, but if addPropSchema was Nothing, then
            -- addProps is bottom, and k is in _schemaRequired. We handle this situation
            -- below and short-circuit the entire Properties condition to bottom
            pure (addProps, fromMaybe fakeSchema addPropSchema)
    pure (k, Property (k `elem` _schemaRequired) f psch)
  let allBottom f = getAll $
        foldType $ \_ ty -> case getJsonFormula $ ty f of
          BottomDNF -> All True
          _ -> All False
      allTop f = getAll $
        foldType $ \_ ty -> case getJsonFormula $ ty f of
          TopDNF -> All True
          _ -> All False
      -- remove optional fields whose schemata match that of additional props
      propMap = M.filter (\p -> propRequired p || propFormula p /= addProps) $ M.fromList propList
      propertiesClause
        | any (\p -> propRequired p && allBottom (propFormula p)) propMap =
          bottom -- if any required field has unsatisfiable schema
        | M.null propMap
          , allTop addProps =
          top -- if all fields are optional and have trivial schemata
        | otherwise =
          top
            { forObject = singletonFormula $ Properties propMap addProps addPropSchema
            }

      maxPropertiesClause = case _schemaMaxProperties of
        Nothing -> top
        Just n ->
          top
            { forObject = singletonFormula $ MaxProperties n
            }

      minPropertiesClause = case _schemaMinProperties of
        Nothing -> top
        Just n ->
          top
            { forObject = singletonFormula $ MinProperties n
            }

      nullableClause
        | Just True <- _schemaNullable =
          bottom
            { forNull = singletonFormula $ Exactly TNull
            }
        | otherwise = bottom

  pure $
    nullableClause
      \/ meets
        ( allClauses
            <> [ anyClause
               , oneClause
               , typeClause
               , enumClause
               , maximumClause
               , minimumClause
               , multipleOfClause
               , formatClause
               , maxLengthClause
               , minLengthClause
               , patternClause
               , itemsClause
               , maxItemsClause
               , minItemsClause
               , uniqueItemsClause
               , propertiesClause
               , maxPropertiesClause
               , minPropertiesClause
               ]
        )

{- TODO: ReadOnly/WriteOnly #68 -}

checkOneOfDisjoint :: MonadProcess m => [Traced (Referenced Schema)] -> m Bool
checkOneOfDisjoint schs = do
  defs <- R.ask
  pure $ case selectPartition $ joins $ runPartitionM defs $ traverse partitionRefSchema schs of
    Nothing -> False
    Just (loc, parts) ->
      let intersects part sch = case runIntersectionM defs $ intersectRefSchema loc part sch of
            Disjoint -> False
            _ -> True
       in all (\part -> 1 >= length (filter (intersects part) schs)) parts
  where

runProcessM :: Traced (Definitions Schema) -> ProcessM a -> (a, P.PathsPrefixTree Behave AnIssue 'SchemaLevel)
runProcessM defs = runWriter . (`runReaderT` defs) . runMemo ()

schemaToFormula ::
  Traced (Definitions Schema) ->
  Traced Schema ->
  (ForeachType JsonFormula, P.PathsPrefixTree Behave AnIssue 'SchemaLevel)
schemaToFormula defs rs = runProcessM defs $ processSchema rs
