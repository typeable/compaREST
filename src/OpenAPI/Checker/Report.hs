module OpenAPI.Checker.Report
  ( generateReport
  )
where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
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
  { sourceJets :: [SomeReportJet Behave]
  , headerLevel :: Int
  }

type ReportMonad = ReaderT ReportState (Writer Blocks)

runReportMonad :: [SomeReportJet Behave] -> ReportMonad () -> Blocks
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

showErrs :: Typeable a => P.PathsPrefixTree Behave AnIssue a -> ReportMonad ()
showErrs x@(P.PathsPrefixNode currentIssues _) = do
  jts <- asks sourceJets
  for_ currentIssues $ \(AnIssue i) -> tell . describeIssue $ i
  unfoldM x (observeSomeJetShowErrs <$> jts) $ \(P.PathsPrefixNode _ subIssues) -> do
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

observeSomeJetShowErrs
  :: forall a.
  Typeable a
  => SomeReportJet Behave
  -> P.PathsPrefixTree Behave AnIssue a
  -> ReportMonad (P.PathsPrefixTree Behave AnIssue a)
observeSomeJetShowErrs (SomeReportJet (Proxy :: Proxy a') f) x
  | Just Refl <- eqT @a @a' = observeJetShowErrs f x
observeSomeJetShowErrs _ x = pure x

observeJetShowErrs :: ReportJet Behave a -> P.PathsPrefixTree Behave AnIssue a -> ReportMonad (P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs jet (P.PathsPrefixNode currentIssues subIssues) = do
  rest <- fmap (fold . join) $
    for subIssues $ \(WrapTypeable (AStep m)) -> fmap catMaybes $
      for (M.toList m) $ \(bhv, subErrs) ->
        case applyReportJet jet bhv of
          Just (Left h) -> do
            smartHeader h
            incrementHeaders $ showErrs subErrs
            return Nothing
          Just (Right jet') -> do
            rest <- observeJetShowErrs jet' subErrs
            return $ Just $ embed (step bhv) rest
          Nothing -> return $ Just $ embed (step bhv) subErrs
  return $ PathsPrefixNode currentIssues mempty <> rest

class ConstructReportJet f a b c where
  constructReportJet :: (f a b -> c) -> ReportJet f a

instance (ConstructReportJet f b c d, Typeable b) => ConstructReportJet f a b (f b c -> d) where
  constructReportJet f = ReportJet Proxy $ \x -> constructReportJet $ f x

instance Typeable b => ConstructReportJet f a b Inlines where
  constructReportJet f = TerminalJet Proxy f

constructSomeReportJet :: (ConstructReportJet f a b c, Typeable a) => (f a b -> c) -> SomeReportJet f
constructSomeReportJet = SomeReportJet Proxy . constructReportJet

data ReportJet f a where
  ReportJet :: Typeable b => Proxy b -> (f a b -> ReportJet f b) -> ReportJet f a
  TerminalJet :: Typeable b => Proxy b -> (f a b -> Inlines) -> ReportJet f a

data SomeReportJet f where
  SomeReportJet :: Typeable a => Proxy a -> ReportJet f a -> SomeReportJet f

applyReportJet :: forall f a b. Typeable b => ReportJet f a -> f a b -> Maybe (Either Inlines (ReportJet f b))
applyReportJet (TerminalJet (Proxy :: Proxy b') f) x = eqT @b @b' <&> \Refl -> Left $ f x
applyReportJet (ReportJet (Proxy :: Proxy b') f) x = eqT @b @b' <&> \Refl -> Right $ f x

incrementHeaders :: ReportMonad x -> ReportMonad x
incrementHeaders m = do
  l <- asks headerLevel
  local (\x -> x {headerLevel = l + 1}) m

jets :: [SomeReportJet Behave]
jets =
  [ constructSomeReportJet $ \p@(AtPath _) op@(InOperation _) ->
      strong (describeBehaviour op) <> " " <> describeBehaviour p :: Inlines
  , constructSomeReportJet $ \InRequest InPayload PayloadSchema -> "JSON Request" :: Inlines
  , constructSomeReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
      "JSON Response – " <> str (T.pack . show $ c) :: Inlines
  ]
