module Data.OpenApi.Compare.Report
  ( generateReport,
    CheckerOutput (..),
    ReportInput (..),
    segregateIssues,
    ReportStatus (..),
    Pandoc,
    ReportConfig (..),
    ReportTreeStyle (..),
    ReportMode (..),
  )
where

import Control.Monad.Free hiding (unfoldM)
import Data.Aeson (ToJSON)
import Data.Default
import Data.Either
import Data.Function
import Data.Functor
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OM
import Data.Maybe
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Paths
import Data.OpenApi.Compare.PathsPrefixTree hiding (empty)
import qualified Data.OpenApi.Compare.PathsPrefixTree as P hiding (empty)
import Data.OpenApi.Compare.Report.Jet
import Data.OpenApi.Compare.Subtree (invertIssueOrientationP)
import Data.OpenApi.Compare.Validate.OpenApi
import Data.OpenApi.Compare.Validate.Schema.Issues
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.OpenUnion
import Data.OpenUnion.Extra
import Data.Set
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.TypeRepMap hiding (empty)
import Data.Typeable
import Generic.Data
import Text.Pandoc.Builder

type Changes = P.PathsPrefixTree Behave AnIssue 'APILevel

data CheckerOutput = CheckerOutput
  { forwardChanges :: Changes
  , backwardChanges :: Changes
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically CheckerOutput)
  deriving anyclass (ToJSON)

data ReportInput = ReportInput
  { -- | forward 'CertainIssue', 'ProbablyIssue' and 'Comment'
    breakingChanges :: Changes
  , -- | backward 'CertainIssue', 'ProbablyIssue' and 'Comment', except those shadowed by 'relatedIssues'
    nonBreakingChanges :: Changes
  , -- | forward and backward 'Unsupported' (assumed to be the same anyway)
    unsupportedChanges :: Changes
  , -- | forward and backward 'SchemaInvalid' (assumed to be the same anyway)
    schemaIssues :: Changes
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically ReportInput)
  deriving anyclass (ToJSON)

segregateIssues :: CheckerOutput -> ReportInput
segregateIssues CheckerOutput {forwardChanges = fwd, backwardChanges = bck} =
  ReportInput
    { breakingChanges = P.filter isBreaking fwd
    , nonBreakingChanges = invertIssueOrientationP $ P.filterWithKey isNonBreaking bck
    , unsupportedChanges = P.filter isUnsupported fwd <> P.filter isUnsupported bck
    , schemaIssues = P.filter isSchemaIssue fwd <> P.filter isSchemaIssue bck
    }
  where
    isBreaking i = anIssueKind i `elem` [CertainIssue, ProbablyIssue, Comment]
    isNonBreaking :: Paths Behave 'APILevel a -> AnIssue a -> Bool
    isNonBreaking xs i = isBreaking i && all (\j -> not $ relatedAnIssues i j) (P.lookup xs fwd)
    isUnsupported i = anIssueKind i == Unsupported
    isSchemaIssue i = anIssueKind i == SchemaInvalid

data ReportStatus
  = BreakingChanges
  | NoBreakingChanges
  | -- | All changes that could be breaking are unsupported – we don't know if
    -- there actually are any breaking changes.
    OnlyUnsupportedChanges

data ReportMode = OnlyErrors | All
  deriving stock (Eq)

data ReportConfig = ReportConfig
  { treeStyle :: ReportTreeStyle
  , reportMode :: ReportMode
  }

instance Default ReportConfig where
  def =
    ReportConfig
      { treeStyle = HeadersTreeStyle
      , reportMode = All
      }

data ReportTreeStyle = HeadersTreeStyle | FoldingBlockquotesTreeStyle

twoRowTable :: [(Inlines, Inlines)] -> Blocks
twoRowTable x = simpleTable (para . fst <$> x) [para . snd <$> x]

generateReport :: ReportConfig -> ReportInput -> (Blocks, ReportStatus)
generateReport cfg inp =
  let schemaIssuesPresent = not $ P.null $ schemaIssues inp
      breakingChangesPresent = not $ P.null $ breakingChanges inp
      nonBreakingChangesPresent = not $ P.null $ nonBreakingChanges inp
      unsupportedChangesPresent = not $ P.null $ unsupportedChanges inp
      nonBreakingChangesShown = case reportMode cfg of
        All -> True
        OnlyErrors -> False
      builder = buildReport cfg
      report =
        header 1 "Summary"
          <> twoRowTable
            ( when'
                schemaIssuesPresent
                [
                  ( refOpt schemaIssuesPresent schemaIssuesId "‼️ Schema issues"
                  , show' $ P.size $ schemaIssues inp
                  )
                ]
                ++ [
                     ( refOpt breakingChangesPresent breakingChangesId "❌ Breaking changes"
                     , show' $ P.size $ breakingChanges inp
                     )
                   ]
                ++ when'
                  nonBreakingChangesShown
                  [
                    ( refOpt nonBreakingChangesPresent nonBreakingChangesId "⚠️ Non-breaking changes"
                    , show' $ P.size $ nonBreakingChanges inp
                    )
                  ]
                ++ when'
                  unsupportedChangesPresent
                  [
                    ( refOpt unsupportedChangesPresent unsupportedChangesId "❓ Unsupported feature changes"
                    , show' $ P.size $ unsupportedChanges inp
                    )
                  ]
            )
          <> when'
            schemaIssuesPresent
            ( header 1 (anchor schemaIssuesId <> "‼️ Schema issues")
                <> builder (showErrs $ schemaIssues inp)
            )
          <> when'
            breakingChangesPresent
            ( header 1 (anchor breakingChangesId <> "❌ Breaking changes")
                <> builder (showErrs $ breakingChanges inp)
            )
          <> when'
            (nonBreakingChangesPresent && nonBreakingChangesShown)
            ( header 1 (anchor nonBreakingChangesId <> "⚠️ Non-breaking changes")
                <> builder (showErrs $ nonBreakingChanges inp)
            )
          <> when'
            unsupportedChangesPresent
            ( header 1 (anchor unsupportedChangesId <> "❓ Unsupported feature changes")
                <> builder (showErrs $ unsupportedChanges inp)
            )
      status =
        if
            | breakingChangesPresent -> BreakingChanges
            | unsupportedChangesPresent -> OnlyUnsupportedChanges
            | otherwise -> NoBreakingChanges
   in (report, status)
  where
    anchor :: Text -> Inlines
    anchor a = spanWith (a, [], []) mempty

    refOpt :: Bool -> Text -> Inlines -> Inlines
    refOpt False _ i = i
    refOpt True a i = link ("#" <> a) "" i

    breakingChangesId, nonBreakingChangesId, unsupportedChangesId, schemaIssuesId :: Text
    breakingChangesId = "breaking-changes"
    unsupportedChangesId = "unsupported-changes"
    nonBreakingChangesId = "non-breaking-changes"
    schemaIssuesId = "schema-issues"

    when' :: Monoid m => Bool -> m -> m
    when' True m = m
    when' False _ = mempty

showErrs :: forall a. Typeable a => P.PathsPrefixTree Behave AnIssue a -> Report
showErrs x@(P.PathsPrefixNode currentIssues _) =
  let -- Extract this pattern if more cases like this arise
      ( removedPaths :: Maybe (Orientation, [Issue 'APILevel])
        , otherIssues :: Set (AnIssue a)
        ) = case eqT @a @ 'APILevel of
          Just Refl
            | (S.toList -> p@((AnIssue ori _) : _), o) <-
                S.partition
                  ( \((AnIssue _ u)) -> case u of
                      NoPathsMatched {} -> True
                      AllPathsFailed {} -> True
                  )
                  currentIssues ->
              let p' = p <&> (\(AnIssue _ i) -> i)
               in (Just (ori, p'), o)
          _ -> (Nothing, currentIssues)
      issues = singletonBody $ case S.toList otherIssues of
        [AnIssue ori i] -> describeIssue ori i
        ii -> orderedList $ ii <&> (\(AnIssue ori i) -> describeIssue ori i)
      paths = case removedPaths of
        Just (ori, ps) -> do
          singletonHeader
            ( case ori of
                Forward -> "Removed paths"
                Backward -> "Added paths"
            )
            $ singletonBody $
              bulletList $
                ps <&> \case
                  (NoPathsMatched p) -> para . code $ T.pack p
                  (AllPathsFailed p) -> para . code $ T.pack p
        Nothing -> mempty
      rest = unfoldFunctions x (observeJetShowErrs <$> jets) $ \(P.PathsPrefixNode _ subIssues) -> do
        flip foldMap subIssues $ \(WrapTypeable (AStep m)) ->
          flip foldMap (M.toList m) $ \(bhv, subErrors) ->
            if P.null subErrors
              then mempty
              else singletonHeader (describeBehavior bhv) $ showErrs subErrors
   in issues <> paths <> rest

unfoldFunctions :: forall m a. (Monoid m, Eq a) => a -> [a -> (m, a)] -> (a -> m) -> m
unfoldFunctions initA fs g = unfoldFunctions' initA fs
  where
    unfoldFunctions' :: a -> [a -> (m, a)] -> m
    unfoldFunctions' a [] | a == initA = g a
    unfoldFunctions' a [] = unfoldFunctions a fs g
    unfoldFunctions' a (f : ff) =
      let (m, a') = f a
       in unfoldFunctions' a' ff <> m

jets :: [ReportJet' Behave (Maybe Inlines)]
jets =
  unwrapReportJetResult
    <$> [ constructReportJet $
            curry $ \case
              (OfType Object, p@(InPartition _)) -> Just $ describeBehavior p :: Maybe Inlines
              _ -> Nothing
        , constructReportJet jsonPathJet
        , constructReportJet $ \p@(AtPath _) op@(InOperation _) ->
            strong (describeBehavior op) <> " " <> describeBehavior p :: Inlines
        , constructReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
            "⬅️☁️ JSON Response – " <> str (T.pack . show $ c) :: Inlines
        , constructReportJet $ \InRequest InPayload PayloadSchema -> "➡️☁️ JSON Request" :: Inlines
        ]
  where
    unwrapReportJetResult :: ReportJetResult Behave x -> ReportJet' Behave x
    unwrapReportJetResult (Pure _) = error "There really shouldn't be any results here."
    unwrapReportJetResult (Free f) = f

    jsonPathJet ::
      NonEmpty
        ( Union
            '[ Behave 'SchemaLevel 'TypedSchemaLevel
             , Behave 'TypedSchemaLevel 'SchemaLevel
             ]
        ) ->
      Inlines
    jsonPathJet x = code $ "$" <> showParts (NE.toList x)
      where
        showParts ::
          [ Union
              '[ Behave 'SchemaLevel 'TypedSchemaLevel
               , Behave 'TypedSchemaLevel 'SchemaLevel
               ]
          ] ->
          Text
        showParts [] = mempty
        showParts (SingletonUnion (OfType Object) : xs@((SingletonUnion (InProperty _)) : _)) = showParts xs
        showParts (SingletonUnion (OfType Object) : xs@((SingletonUnion InAdditionalProperty) : _)) = showParts xs
        showParts (SingletonUnion (OfType Array) : xs@(SingletonUnion InItems : _)) = showParts xs
        showParts (SingletonUnion (OfType Array) : xs@(SingletonUnion (InItem _) : _)) = showParts xs
        showParts (y : ys) =
          ( (\(OfType t) -> "(" <> describeJSONType t <> ")")
              @@> ( \case
                      InItems -> "[*]"
                      InItem i -> "[" <> T.pack (show i) <> "]"
                      InProperty p -> "." <> p
                      InAdditionalProperty -> ".*"
                  )
              @@> typesExhausted
          )
            y
            <> showParts ys

observeJetShowErrs ::
  ReportJet' Behave (Maybe Inlines) ->
  P.PathsPrefixTree Behave AnIssue a ->
  (Report, P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs jet p = case observeJetShowErrs' jet p of
  Just m -> m
  Nothing -> (mempty, p)

observeJetShowErrs' ::
  forall a.
  ReportJet' Behave (Maybe Inlines) ->
  P.PathsPrefixTree Behave AnIssue a ->
  Maybe (Report, P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs' (ReportJet jet) (P.PathsPrefixNode currentIssues subIssues) =
  let results =
        subIssues >>= \(WrapTypeable (AStep m)) ->
          M.toList m <&> \(bhv, subErrs) ->
            maybe (Left $ embed (step bhv) subErrs) Right . listToMaybe $
              jet @_ @_ @[] bhv
                & mapMaybe
                  ( \case
                      Free jet' -> fmap (embed $ step bhv) <$> observeJetShowErrs' jet' subErrs
                      Pure (Just h) ->
                        if P.null subErrs
                          then Just mempty
                          else Just (singletonHeader h (showErrs subErrs), mempty)
                      Pure Nothing -> Nothing
                  )
   in (fmap . fmap) (PathsPrefixNode currentIssues mempty <>) $
        if any isRight results
          then
            Just $
              foldMap
                ( \case
                    Left e -> (mempty, e)
                    Right m -> m
                )
                results
          else Nothing

data Report = Report {headers :: OMap Inlines Report, body :: Blocks}
  deriving stock (Generic)

instance Semigroup Report where
  (Report headers1 b1) <> (Report headers2 b2) = Report (OM.unionWithL (const (<>)) headers1 headers2) (b1 <> b2)

instance Monoid Report where
  mempty = Report OM.empty mempty

buildReport :: ReportConfig -> Report -> Blocks
buildReport cfg = case treeStyle cfg of
  HeadersTreeStyle -> headerStyleBuilder 2
  FoldingBlockquotesTreeStyle -> foldingStyleBuilder
  where
    headerStyleBuilder :: HeaderLevel -> Report -> Blocks
    headerStyleBuilder level rprt =
      body rprt
        <> foldOMapWithKey
          (headers rprt)
          ( \k v ->
              header level k <> subBuilder v
          )
      where
        subBuilder = headerStyleBuilder (level + 1)

    foldingStyleBuilder :: Report -> Blocks
    foldingStyleBuilder rprt =
      body rprt
        <> foldOMapWithKey
          (headers rprt)
          ( \k v ->
              if (OM.size . headers $ rprt) < 2
                then para k <> blockQuote (subBuilder v)
                else
                  rawHtml "<details>"
                    <> rawHtml "<summary>"
                    <> plain k
                    <> rawHtml "</summary>"
                    <> blockQuote (subBuilder v)
                    <> rawHtml "</details>"
          )
      where
        subBuilder = foldingStyleBuilder

    rawHtml = rawBlock "html"

type HeaderLevel = Int

singletonHeader :: Inlines -> Report -> Report
singletonHeader i b = Report (OM.singleton (i, b)) mempty

singletonBody :: Blocks -> Report
singletonBody = Report OM.empty

show' :: Show x => x -> Inlines
show' = str . T.pack . show

foldOMapWithKey :: Monoid m => OMap k v -> (k -> v -> m) -> m
foldOMapWithKey m f = foldMap (uncurry f) $ OM.assocs m
