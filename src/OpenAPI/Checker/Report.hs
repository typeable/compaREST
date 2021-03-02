module OpenAPI.Checker.Report where

import           Data.Map.Strict                as M
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
import           Data.Semigroup.Generic
import           Data.Sequence                  (Seq)
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
  { paths :: Seq (Diff PathItemTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

newtype PathItemTree = PathItemTree
  { operations :: Seq (Diff OperationTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance Node PathItemTree where
  type Parent PathItemTree = ReportTree
  type Key PathItemTree = FilePath
  type Original PathItemTree = PathItem
  nest sd = ReportTree sd

newtype OperationTree = OperationTree
  { parameters :: Seq (Diff ParamTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance Node OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  type Original OperationTree = Operation
  nest sd = PathItemTree sd

data OperationName
  = Get | Put | Post | Delete | Options | Head | Patch | Trace
  deriving (Eq, Ord, Show, Generic)

data ServerTree = ServerTree
  deriving (Eq, Ord, Show, Generic)

data ParamTree = ParamTree
  { required        :: Errorable ParamRequiredTree
  , allowEmptyValue :: Errorable ParamAllowEmptyValueTree
  , allowReserved   :: Errorable ParamAllowReservedTree
  , schema          :: Maybe (Errorable ParamSchemaTree)
  , style           :: Errorable ParamStyleTree
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup ParamTree where
  (<>) = genericMappend
instance Monoid ParamTree where
  mappend = (<>)
  mempty = genericMempty

instance Nested ParamTree where
  type Key ParamTree = ParamKey
  type Parent ParamTree = OperationTree
  nest key p = OperationTree $ M.singleton key p

deriving instance Ord ParamLocation

data ParamRequiredTree = ParamRequiredTree
  deriving (Eq, Ord, Show, Generic)

data ParamAllowEmptyValueTree = ParamAllowEmptyValueTree
  deriving (Eq, Ord, Show, Generic)

data ParamAllowReservedTree = ParamAllowReservedTree
  deriving (Eq, Ord, Show, Generic)

data ParamSchemaTree = ParamSchemaTree
  deriving (Eq, Ord, Show, Generic)

data ParamStyleTree = ParamStyleTree
  deriving (Eq, Ord, Show, Generic)

data ParamKey = ParamKey
  { name    :: Text
  , paramIn :: ParamLocation
  } deriving (Eq, Ord, Show, Generic)

getParamKey :: Param -> ParamKey
getParamKey p = ParamKey
  { name = _paramName p
  , paramIn = _paramIn p
  }
