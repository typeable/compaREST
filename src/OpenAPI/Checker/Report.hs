module OpenAPI.Checker.Report
  ( generateReport
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.OpenUnion
import Data.OpenUnion.Extra
import qualified Data.Text as T
import Data.Traversable
import Data.TypeRepMap hiding (empty)
import Data.Typeable
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Paths
import OpenAPI.Checker.PathsPrefixTree hiding (empty)
import qualified OpenAPI.Checker.PathsPrefixTree as P hiding (empty)
import OpenAPI.Checker.Validate.OpenApi
import Text.Pandoc.Builder

generateReport :: Either (P.PathsPrefixTree Behave AnIssue 'APILevel) () -> Pandoc
generateReport (Right ()) = doc $ header 1 "No breaking changes found ✨"
generateReport (Left errs) = doc $ runReportMonad jets $ showErrs errs

data ReportState = ReportState
  { sourceJets :: [ReportJet Behave]
  , headerLevel :: Int
  }

type ReportMonad = ReaderT ReportState (Writer Blocks)

runReportMonad :: [ReportJet Behave] -> ReportMonad () -> Blocks
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

observeJetShowErrs :: ReportJet Behave -> P.PathsPrefixTree Behave AnIssue a -> ReportMonad (P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs (ReportJet jet) (P.PathsPrefixNode currentIssues subIssues) = do
  rest <- fmap (fold . join) $
    for subIssues $ \(WrapTypeable (AStep m)) -> fmap catMaybes $
      for (M.toList m) $ \(bhv, subErrs) ->
        case jet bhv of
          Just (ReportJetResult h) -> do
            smartHeader h
            incrementHeaders $ showErrs subErrs
            return Nothing
          Just (ReportContinuation jet') -> do
            rest <- observeJetShowErrs jet' subErrs
            return $ Just $ embed (step bhv) rest
          Nothing -> return $ Just $ embed (step bhv) subErrs
  return $ PathsPrefixNode currentIssues mempty <> rest

-- | A "jet" is a way of simplifying expressions from "outside". The "jetted"
-- expressions should still be completely valid and correct without the jets.
-- Jets just make the expression more "optimized" by identifying patterns and
-- replacing the expressions with "better" ones that have the same sematics.
--
-- The tem "jet" in this context was introduced in the Urbit project:
--   https://urbit.org/docs/vere/jetting/
--
-- The pattern fits well for simplifying 'Behaviour' tree paths.
class ConstructReportJet x f where
  constructReportJet :: x -> ReportJetResult f

instance (ConstructReportJet b f, JetArg a, Typeable f) => ConstructReportJet (a -> b) f where
  constructReportJet f = ReportContinuation $ ReportJet $ \u -> constructReportJet . f <$> consumeJetArg u

instance ConstructReportJet Inlines f where
  constructReportJet x = ReportJetResult x

class JetArg a where
  consumeJetArg :: (Typeable x, Alternative m) => x -> m a

instance Typeable (f a b) => JetArg (f a b) where
  consumeJetArg (x :: x) = case eqT @(f a b) @x of
    Nothing -> empty
    Just Refl -> pure x

instance TryLiftUnion xs => JetArg (Union xs) where
  consumeJetArg = tryLiftUnion

data ReportJetResult f
  = ReportJetResult Inlines
  | ReportContinuation (ReportJet f)

newtype ReportJet f = ReportJet (forall a b m. (Typeable a, Typeable b, Alternative m) => (f a b -> m (ReportJetResult f)))

incrementHeaders :: ReportMonad x -> ReportMonad x
incrementHeaders m = do
  l <- asks headerLevel
  local (\x -> x {headerLevel = l + 1}) m

jets :: [ReportJet Behave]
jets =
  unwrapReportJetResult
    <$> [ constructReportJet $ \p@(AtPath _) op@(InOperation _) ->
            strong (describeBehaviour op) <> " " <> describeBehaviour p :: Inlines
        , constructReportJet $ \InRequest InPayload PayloadSchema -> "JSON Request" :: Inlines
        , constructReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
            "JSON Response – " <> str (T.pack . show $ c) :: Inlines
        ]
  where
    unwrapReportJetResult (ReportJetResult _) = error "There really shouldn't be any results here."
    unwrapReportJetResult (ReportContinuation f) = f
