module OpenAPI.Checker.Report where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Report = Report
  { status :: Status
  , tree   :: ReportTree
  } deriving (Eq, Ord, Show, Generic)

data Status = Success | Fail Text
  deriving (Eq, Ord, Show, Generic)

type Errorable = Either Text

type Path = FilePath -- From the library

data ReportTree = ReportTree
  { paths :: [PathTree]
  } deriving (Eq, Ord, Show, Generic)

data PathTree = PathTree
  { path     :: Path
  , pathItem :: Errorable PathItemTree
  } deriving (Eq, Ord, Show, Generic)

data PathItemTree = PathItemTree
  deriving (Eq, Ord, Show, Generic)

printReport :: Report -> IO ()
printReport = error "FIXME: printReport not implemented"
