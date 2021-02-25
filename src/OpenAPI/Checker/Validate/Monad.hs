module OpenAPI.Checker.Validate.Monad where

import           Data.Text              (Text)
import           OpenAPI.Checker.Report

data ReportTreeT a = ReportTreeT

instance Functor ReportTreeT
instance Applicative ReportTreeT
instance Monad ReportTreeT


runReportTreeT :: ReportTreeT a -> (ReportTree, a)
runReportTreeT = error "FIXME: runReportTree not implemented"

pathError :: Text -> ReportTreeT a
pathError = error "FIXME: pathError not implemented"

follow
  :: (Traversable f)
  => f (Path, ReportTreeT a)
  -> ReportTreeT (f a)
follow = error "FIXME: follow not implemented"
