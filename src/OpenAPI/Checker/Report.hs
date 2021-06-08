module OpenAPI.Checker.Report
  ( generateReport
  )
where

import Control.Applicative
import Control.Monad.Free hiding (unfoldM)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.OpenUnion
import Data.OpenUnion.Extra
import qualified Data.Text as T
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
  { sourceJets :: [ReportJet' Behave]
  , headerLevel :: Int
  }

type ReportMonad = ReaderT ReportState (Writer Blocks)

runReportMonad :: [ReportJet' Behave] -> ReportMonad () -> Blocks
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

observeJetShowErrs
  :: ReportJet' Behave
  -> P.PathsPrefixTree Behave AnIssue a
  -> ReportMonad (P.PathsPrefixTree Behave AnIssue a)
observeJetShowErrs jet p = case observeJetShowErrs' jet p of
  Just m -> m
  Nothing -> pure p

observeJetShowErrs'
  :: forall a.
     ReportJet' Behave
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

-- | A "jet" is a way of simplifying expressions from "outside". The "jetted"
-- expressions should still be completely valid and correct without the jets.
-- Jets just make the expression more "optimized" by identifying patterns and
-- replacing the expressions with "better" ones that have the same sematics.
--
-- The term "jet" in this context was introduced in the Urbit project:
--   https://urbit.org/docs/vere/jetting/
--
-- The pattern fits well for simplifying 'Behaviour' tree paths.
class ConstructReportJet x f where
  constructReportJet :: (Alternative m, Monad m) => x -> m (ReportJetResult f Inlines)

instance (ConstructReportJet b f, JetArg a) => ConstructReportJet (a -> b) f where
  constructReportJet f = do
    arg <- consumeJetArg
    let res :: ReportJetResult f b = f <$> arg
    embedJetResult res constructReportJet

instance ConstructReportJet Inlines f where
  constructReportJet x = pure $ Pure x

class JetArg a where
  consumeJetArg :: Alternative m => m (ReportJetResult f a)

instance Typeable (f a b) => JetArg (f a b) where
  consumeJetArg = pure $
    Free $
      ReportJet $ \(x :: x) ->
        case eqT @(f a b) @x of
          Nothing -> empty
          Just Refl -> pure $ Pure x

instance TryLiftUnion xs => JetArg (Union xs) where
  consumeJetArg = pure $ Free $ ReportJet $ fmap Pure . tryLiftUnion

type ReportJetResult f = Free (ReportJet f)

embedJetResult
  :: (Alternative n, Monad n)
  => ReportJetResult f a
  -> (forall m. (Alternative m, Monad m) => a -> m (ReportJetResult f b))
  -> n (ReportJetResult f b)
embedJetResult (Pure x) f = f x
embedJetResult (Free (ReportJet g)) f = pure $ Free $ ReportJet $ fmap (>>= (`embedJetResult` f)) g

newtype ReportJet f x = ReportJet (forall a b m. (Typeable (f a b), Alternative m, Monad m) => f a b -> m x)
  deriving stock (Functor)

type ReportJet' f = ReportJet f (Free (ReportJet Behave) Inlines)

incrementHeaders :: ReportMonad x -> ReportMonad x
incrementHeaders m = do
  l <- asks headerLevel
  local (\x -> x {headerLevel = l + 1}) m

jets :: [ReportJet' Behave]
jets =
  fmap unwrapReportJetResult . join $
    [ constructReportJet $ \p@(AtPath _) op@(InOperation _) ->
        strong (describeBehaviour op) <> " " <> describeBehaviour p :: Inlines
    , constructReportJet $ \InRequest InPayload PayloadSchema -> "JSON Request" :: Inlines
    , constructReportJet $ \(WithStatusCode c) ResponsePayload PayloadSchema ->
        "JSON Response – " <> str (T.pack . show $ c) :: Inlines
    ]
  where
    unwrapReportJetResult :: ReportJetResult Behave Inlines -> ReportJet' Behave
    unwrapReportJetResult (Pure _) = error "There really shouldn't be any results here."
    unwrapReportJetResult (Free f) = f
