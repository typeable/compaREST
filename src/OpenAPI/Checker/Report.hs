module OpenAPI.Checker.Report where

import           Data.Map.Strict                as M
import           Data.Monoid.Generic
import           Data.Semigroup.Generic
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

newtype ReportTree = ReportTree
  { paths :: Map Path (Errorable PathItemTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

data PathItemTree = PathItemTree
  { operations :: Map OperationName (Errorable OperationTree)
  , server     :: Maybe (Errorable ServerTree)
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup PathItemTree where
  (<>) = genericMappend

instance Monoid PathItemTree where
  mempty = genericMempty

instance Nested PathItemTree where
  type Parent PathItemTree = ReportTree
  type Key PathItemTree = Path
  nest key p = ReportTree $ M.singleton key p

data OperationName
  = Get | Put | Post | Delete | Options | Head | Patch | Trace
  deriving (Eq, Ord, Show, Generic)

data OperationTree = OperationTree
  deriving (Eq, Ord, Show, Generic)

instance Semigroup OperationTree where
  _ <> _ = OperationTree

instance Monoid OperationTree where
  mempty = OperationTree

instance Nested OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  nest key p = PathItemTree (M.singleton key p) mempty

data ServerTree = ServerTree
  deriving (Eq, Ord, Show, Generic)
