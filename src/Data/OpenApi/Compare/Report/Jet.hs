module Data.OpenApi.Compare.Report.Jet
  ( ReportJet (..),
    ReportJet',
    ConstructReportJet (..),
    ReportJetResult,
  )
where

import Control.Applicative
import Control.Monad.Free
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.OpenUnion
import Data.OpenUnion.Extra
import Data.Typeable
import Text.Pandoc.Builder

-- | A "jet" is a way of simplifying expressions from "outside". The "jetted"
-- expressions should still be completely valid and correct without the jets.
-- Jets just make the expression more "optimized" by identifying patterns and
-- replacing the expressions with "better" ones that have the same sematics.
--
-- The term "jet" in this context was introduced in the Urbit project:
--   https://urbit.org/docs/vere/jetting/
--
-- The pattern fits well for simplifying 'Behavior' tree paths.
class ConstructReportJet x f where
  constructReportJet :: x -> ReportJetResult f (Maybe Inlines)

instance (ConstructReportJet b f, JetArg a) => ConstructReportJet (a -> b) f where
  constructReportJet f = Free (fmap f <$> consumeJetArg @a) >>= constructReportJet

instance ConstructReportJet (Maybe Inlines) f where
  constructReportJet x = Pure x

instance ConstructReportJet Inlines f where
  constructReportJet x = Pure $ Just x

class JetArg a where
  consumeJetArg :: ReportJet' f a

instance Typeable (f a b) => JetArg (f a b) where
  consumeJetArg =
    ReportJet $ \(x :: x) ->
      case eqT @(f a b) @x of
        Nothing -> empty
        Just Refl -> pure $ Pure x

instance TryLiftUnion xs => JetArg (Union xs) where
  consumeJetArg = ReportJet $ fmap Pure . tryLiftUnion

instance JetArg x => JetArg (NonEmpty x) where
  consumeJetArg =
    let (ReportJet f) = (consumeJetArg @x)
     in ReportJet $ \a -> do
          u <- f a
          pure (u >>= \y -> Free $ fmap (NE.cons y) <$> consumeJetArg)
            <|> pure (pure <$> u)

type ReportJetResult f = Free (ReportJet f)

-- Not a true 'Applicative'
newtype ReportJet f x
  = ReportJet (forall a b m. (Typeable (f a b), Alternative m, Monad m) => f a b -> m x)
  deriving stock (Functor)

type ReportJet' f a = ReportJet f (Free (ReportJet f) a)
