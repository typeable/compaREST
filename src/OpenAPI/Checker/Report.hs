module OpenAPI.Checker.Report where

import           Data.Map.Strict                as M
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
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

newtype OperationTree = OperationTree
  { parameters :: Map ParamKey (Errorable ParamTree)
  } deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance Nested OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  nest key p = PathItemTree (M.singleton key p) mempty

data ServerTree = ServerTree
  deriving (Eq, Ord, Show, Generic)

data ParamTree = ParamTree
  deriving (Eq, Ord, Show, Generic)

instance Semigroup ParamTree where
  (<>) = error "Not implemented"
instance Monoid ParamTree where
  mappend = (<>)
  mempty = error "Not implemented"

instance Nested ParamTree where
  type Key ParamTree = ParamKey
  type Parent ParamTree = OperationTree
  nest key p = OperationTree $ M.singleton key p

deriving instance Ord ParamLocation

data ParamKey = ParamKey
  { name    :: Text
  , paramIn :: ParamLocation
  } deriving (Eq, Ord, Show, Generic)

getParamKey :: Param -> ParamKey
getParamKey p = ParamKey
  { name = _paramName p
  , paramIn = _paramIn p
  }
