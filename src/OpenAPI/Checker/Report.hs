module OpenAPI.Checker.Report where

import           Control.Lens
import           Data.Generics.Product
import           Data.Map.Strict                as M
import           Data.Monoid
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
import           Data.Semigroup.Generic
import           Data.Sequence                  (Seq)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           OpenAPI.Checker.Report.Orphans
import           OpenAPI.Checker.Validate.Monad

printReport :: Report -> IO ()
printReport = error "FIXME: printReport not implemented"

data Report = Report
  { status :: Status
  , tree   :: ReportTree
  } deriving (Eq, Show, Generic)

data Status = Success | Fail Text
  deriving (Eq, Ord, Show, Generic)

type Path = FilePath -- From the library

newtype ReportTree = ReportTree
  { paths :: Seq (Diff PathItemTree)
  } deriving (Eq, Show, Generic, Semigroup, Monoid)

newtype PathItemTree = PathItemTree
  { operations :: Seq (Diff OperationTree)
  } deriving (Eq, Show, Generic, Semigroup, Monoid)

instance Node PathItemTree where
  type Parent PathItemTree = ReportTree
  type Key PathItemTree = FilePath
  type Original PathItemTree = PathItem
  nest key t = mappend $ ReportTree $ chdiff key t

newtype OperationTree = OperationTree
  { parameters :: Seq (Diff ParamTree)
  } deriving (Eq, Show, Generic, Semigroup, Monoid)

instance Node OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  type Original OperationTree = Operation
  nest key t = field @"operations" <>~ chdiff key t

data OperationName
  = Get | Put | Post | Delete | Options | Head | Patch | Trace
  deriving (Eq, Ord, Show, Generic)

data ServerTree = ServerTree
  deriving (Eq, Show, Generic)

data ParamTree = ParamTree
  { required        :: Compatible
  , allowEmptyValue :: Compatible
  , allowReserved   :: Compatible
  , schema          :: Compatible
  , style           :: Compatible
  } deriving (Eq, Show, Generic)

instance Semigroup ParamTree where
  (<>) = genericMappend
instance Monoid ParamTree where
  mappend = (<>)
  mempty = genericMempty

instance Node ParamTree where
  type Parent ParamTree = OperationTree
  type Key ParamTree = ParamKey
  type Original ParamTree = Param
  nest key t = mappend $ OperationTree $ chdiff key t

data ParamKey = ParamKey
  { name    :: Text
  , paramIn :: ParamLocation
  } deriving (Eq, Ord, Show, Generic)

getParamKey :: Param -> ParamKey
getParamKey p = ParamKey
  { name = _paramName p
  , paramIn = _paramIn p
  }
