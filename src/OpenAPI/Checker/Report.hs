module OpenAPI.Checker.Report
  ( generateReport
  )
where

import Control.Monad.Free hiding (unfoldM)
import Control.Monad.Reader
import Control.Monad.Writer
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.TypeRepMap hiding (empty)
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Paths
import OpenAPI.Checker.PathsPrefixTree hiding (empty)
import qualified OpenAPI.Checker.PathsPrefixTree as P hiding (empty)
import OpenAPI.Checker.Report.Jet
import OpenAPI.Checker.Validate.OpenApi
import OpenAPI.Checker.Validate.Schema
import Text.Pandoc.Builder

generateReport :: Either (P.PathsPrefixTree Behave AnIssue 'APILevel) () -> Pandoc
generateReport (Right ()) = doc $ header 1 "No breaking changes found ✨"
generateReport (Left errs) = doc $ runReportMonad jets $ showErrs errs

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

showErrs :: P.PathsPrefixTree Behave AnIssue a -> ReportMonad ()
showErrs x@(P.PathsPrefixNode currentIssues _) = do
  jts <- asks sourceJets
  for_ currentIssues $ \(AnIssue i) -> tell . describeIssue $ i
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
        , constructReportJet $ \InRequest InPayload PayloadSchema -> "JSON Request" :: Inlines
        , constructReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
            "JSON Response – " <> str (T.pack . show $ c) :: Inlines
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
