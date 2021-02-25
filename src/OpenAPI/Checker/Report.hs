module OpenAPI.Checker.Report where

import           Data.Map.Strict                as M
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           OpenAPI.Checker.Validate.Monad

printReport :: Report -> IO ()
printReport = error "FIXME: printReport not implemented"

data Report = Report
  { status :: Status
  , tree   :: ReportTree
  } deriving (Eq, Ord, Show, Generic)

data Status = Success | Fail Text
  deriving (Eq, Ord, Show, Generic)

type Path = FilePath -- From the library

data ReportTree = ReportTree
  { paths :: Map Path (Errorable PathItemTree)
  } deriving (Eq, Ord, Show, Generic)

newtype PathItemTree = PathItemTree
  { operations :: Map OperationName (Errorable OperationTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance Nested PathItemTree where
  type Parent PathItemTree = ReportTree
  type Key PathItemTree = Path
  nest key p = ReportTree $ M.singleton key p

data OperationName
  = Get | Put | Post | Delete | Options | Head | Patch | Trace
  deriving (Eq, Ord, Show, Generic)

data OperationTree = OperationTree
  deriving (Eq, Ord, Show, Generic)

instance Nested OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  nest key p = PathItemTree $ M.singleton key p
