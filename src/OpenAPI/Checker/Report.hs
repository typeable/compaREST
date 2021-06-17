module OpenAPI.Checker.Report
  ( generateReport
  , ReportInput (..)
  , ReportStatus (..)
  , Pandoc
  )
where

import Control.Monad.Free hiding (unfoldM)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson (ToJSON)
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.OpenUnion
import Data.OpenUnion.Extra
import Data.Set
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.TypeRepMap hiding (empty)
import Data.Typeable
import Generic.Data
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Paths
import OpenAPI.Checker.PathsPrefixTree hiding (empty)
import qualified OpenAPI.Checker.PathsPrefixTree as P hiding (empty)
import OpenAPI.Checker.Report.Jet
import OpenAPI.Checker.Validate.OpenApi
import OpenAPI.Checker.Validate.Schema
import Text.Pandoc.Builder

type Changes = P.PathsPrefixTree Behave AnIssue 'APILevel

data ReportInput = ReportInput
  { breakingChanges :: Changes
  , nonBreakingChanges :: Changes
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically ReportInput)
  deriving anyclass (ToJSON)

data ReportStatus
  = BreakingChanges
  | NoBreakingChanges
  | -- | All changes that could be breaking are unsupported – we don't know if
    -- there actually are any breaking changes.
    OnlyUnsupportedChanges

generateReport :: ReportInput -> (Pandoc, ReportStatus)
generateReport inp =
  let (bUnsupported, breaking) = P.partition (\(AnIssue i) -> issueIsUnsupported i) $ breakingChanges inp
      (nbUnsupported, nonBreaking) = P.partition (\(AnIssue i) -> issueIsUnsupported i) $ nonBreakingChanges inp
      unsupported = bUnsupported <> nbUnsupported
      breakingChangesPresent = not $ P.null breaking
      nonBreakingChangesPresent = not $ P.null nonBreaking
      unsupportedChangesPresent = not $ P.null unsupported
      report = doc $
        runReportMonad jets $ do
          smartHeader "Summary"
          tell $
            simpleTable
              (para
                 <$> [ refOpt breakingChangesPresent breakingChangesId "⚠️ Breaking changes"
                     , refOpt nonBreakingChangesPresent nonBreakingChangesId "🙆 Non-breaking changes"
                     , refOpt unsupportedChangesPresent unsupportedChangesId "🤷 Unsupported feature changes"
                     ])
              [para . show' <$> [P.size breaking, P.size nonBreaking, P.size unsupported]]
          when breakingChangesPresent $ do
            smartHeader $ anchor breakingChangesId <> "⚠️ Breaking changes"
            incrementHeaders $ showErrs breaking
          when nonBreakingChangesPresent $ do
            smartHeader $ anchor nonBreakingChangesId <> "🙆 Non-breaking changes"
            incrementHeaders $ showErrs nonBreaking
          when unsupportedChangesPresent $ do
            smartHeader $ anchor unsupportedChangesId <> "🤷 Unsupported feature changes"
            incrementHeaders $ showErrs unsupported
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

    breakingChangesId, nonBreakingChangesId, unsupportedChangesId :: Text
    breakingChangesId = "breaking-changes"
    unsupportedChangesId = "unsupported-changes"
    nonBreakingChangesId = "non-breaking-changes"

data ReportState = ReportState
  { sourceJets :: [ReportJet' Behave Inlines]
  , headerLevel :: Int
  }

type ReportMonad = ReaderT ReportState (Writer Blocks)

runReportMonad :: [ReportJet' Behave Inlines] -> ReportMonad () -> Blocks
runReportMonad jts =
  execWriter
    . flip
      runReaderT
      ReportState
        { sourceJets = jts
        , headerLevel = 1
        }

smartHeader :: Inlines -> ReportMonad ()
smartHeader i = do
  h <- asks headerLevel
  tell $ header h i

showErrs :: forall a. Typeable a => P.PathsPrefixTree Behave AnIssue a -> ReportMonad ()
showErrs x@(P.PathsPrefixNode currentIssues _) = do
  let -- Extract this pattern if more cases like this arise
      (removedPaths :: [Issue 'APILevel], otherIssues :: Set (AnIssue a)) = case eqT @a @'APILevel of
        Just Refl ->
          let (p, o) =
                S.partition
                  (\(AnIssue u) -> case u of
                     NoPathsMatched {} -> True
                     AllPathsFailed {} -> True)
                  currentIssues
              p' = S.toList p <&> (\(AnIssue i) -> i)
           in (p', o)
        Nothing -> (mempty, currentIssues)
  jts <- asks sourceJets
  for_ otherIssues $ \(AnIssue i) -> tell . describeIssue $ i
  unless ([] == removedPaths) $ do
    smartHeader "Removed paths"
    tell $
      bulletList $
        removedPaths <&> \case
          (NoPathsMatched p) -> para . code $ T.pack p
          (AllPathsFailed p) -> para . code $ T.pack p
  unfoldM x (observeJetShowErrs <$> jts) $ \(P.PathsPrefixNode _ subIssues) -> do
    for_ subIssues $ \(WrapTypeable (AStep m)) ->
      for_ (M.toList m) $ \(bhv, subErrors) -> do
        unless (P.null subErrors) $ do
          smartHeader $ describeBehaviour bhv
          incrementHeaders $ showErrs subErrors

unfoldM :: Monad m => a -> [a -> m a] -> (a -> m ()) -> m ()
unfoldM a [] g = g a
unfoldM a (f : ff) g = do
  a' <- f a
  unfoldM a' ff g

incrementHeaders :: ReportMonad x -> ReportMonad x
incrementHeaders m = do
  l <- asks headerLevel
  local (\x -> x {headerLevel = l + 1}) m

jets :: [ReportJet' Behave Inlines]
jets =
  unwrapReportJetResult
    <$> [ constructReportJet jsonPathJet
        , constructReportJet $ \p@(AtPath _) op@(InOperation _) ->
            strong (describeBehaviour op) <> " " <> describeBehaviour p :: Inlines
        , constructReportJet $ \InRequest InPayload PayloadSchema -> "📱➡️ JSON Request" :: Inlines
        , constructReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
            "📱⬅️ JSON Response – " <> str (T.pack . show $ c) :: Inlines
        ]
  where
    unwrapReportJetResult :: ReportJetResult Behave x -> ReportJet' Behave x
    unwrapReportJetResult (Pure _) = error "There really shouldn't be any results here."
    unwrapReportJetResult (Free f) = f

    jsonPathJet
      :: NonEmpty
           ( Union
               '[ Behave 'SchemaLevel 'TypedSchemaLevel
                , Behave 'TypedSchemaLevel 'SchemaLevel
                ]
           )
      -> Inlines
    jsonPathJet x = code $ "$" <> showParts (NE.toList x)
      where
        showParts
          :: [ Union
                 '[ Behave 'SchemaLevel 'TypedSchemaLevel
                  , Behave 'TypedSchemaLevel 'SchemaLevel
                  ]
             ]
          -> Text
        showParts [] = mempty
        showParts (SingletonUnion (OfType Object) : xs@((SingletonUnion (InProperty _)) : _)) = showParts xs
        showParts (SingletonUnion (OfType Object) : xs@((SingletonUnion InAdditionalProperty) : _)) = showParts xs
        showParts (SingletonUnion (OfType Array) : xs@(SingletonUnion InItems : _)) = showParts xs
        showParts (y : ys) =
          ((\(OfType t) -> "(" <> describeJSONType t <> ")")
             @@> (\case
                    InItems -> "[*]"
                    InProperty p -> "." <> p
                    InAdditionalProperty -> ".*")
             @@> typesExhausted)
            y
            <> showParts ys

observeJetShowErrs
  :: ReportJet' Behave Inlines
  -> P.PathsPrefixTree Behave AnIssue a
  -> ReportMonad (P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs jet p = case observeJetShowErrs' jet p of
  Just m -> m
  Nothing -> pure p

observeJetShowErrs'
  :: forall a.
     ReportJet' Behave Inlines
  -> P.PathsPrefixTree Behave AnIssue a
  -> Maybe (ReportMonad (P.PathsPrefixTree Behave AnIssue a))
observeJetShowErrs' (ReportJet jet) (P.PathsPrefixNode currentIssues subIssues) =
  let results =
        subIssues >>= \(WrapTypeable (AStep m)) ->
          M.toList m <&> \(bhv, subErrs) ->
            maybe (Left $ embed (step bhv) subErrs) Right . listToMaybe $
              jet @_ @_ @[] bhv
                & mapMaybe
                  (\case
                     Free jet' -> fmap (embed $ step bhv) <$> observeJetShowErrs' jet' subErrs
                     Pure h -> Just $ do
                       unless (P.null subErrs) $ do
                         smartHeader h
                         incrementHeaders $ showErrs subErrs
                       return mempty)
   in (fmap . fmap) (PathsPrefixNode currentIssues mempty <>) $
        if any isRight results
          then
            Just $
              catMapM
                (\case
                   Left e -> pure e
                   Right m -> m)
                results
          else Nothing

catMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
catMapM f xs = mconcat <$> mapM f xs

show' :: Show x => x -> Inlines
show' = str . T.pack . show
