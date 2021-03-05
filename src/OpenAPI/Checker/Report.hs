module OpenAPI.Checker.Report where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text           as A
import           Data.Functor
import           Data.Map.Strict                as M
import           Data.Maybe
import           Data.Monoid.Generic
import           Data.OpenApi.Internal
import           Data.Text                      (Text)
import           Deriving.Aeson.Stock
import           OpenAPI.Checker.Validate.Monad
import           Prelude                        as P

printReport :: Report -> IO ()
printReport = error "FIXME: printReport not implemented"

class HasUnsupportedFeature x where
  hasUnsupportedFeature :: x -> Bool

data Report = Report
  { status :: Status
  , tree   :: ReportTree
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake Report

instance HasUnsupportedFeature Report 

instance HasUnsupportedFeature x => HasUnsupportedFeature (Either e x) where
  hasUnsupportedFeature (Left _) = False
  hasUnsupportedFeature (Right x) = hasUnsupportedFeature x

data Status = Success | Fail Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake Status

type Path = FilePath -- From the library

newtype ReportTree = ReportTree
  { paths :: Map Path (Errorable PathItemTree)
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via Snake ReportTree

instance HasUnsupportedFeature ReportTree 

data PathItemTree = PathItemTree
  { operations :: Map OperationName (Errorable OperationTree)
  , server     :: Maybe (Errorable ServerTree)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake PathItemTree

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
  deriving (ToJSON, FromJSON) via Snake OperationName
  deriving anyclass (ToJSONKey, FromJSONKey)

newtype OperationTree = OperationTree
  { parameters :: Map ParamKey (Errorable ParamTree)
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Semigroup, Monoid, FromJSON, ToJSON)

instance Nested OperationTree where
  type Parent OperationTree = PathItemTree
  type Key OperationTree = OperationName
  nest key p = PathItemTree (M.singleton key p) mempty

data ServerTree = ServerTree
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake ServerTree

data ParamTree = ParamTree
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Snake ParamTree

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
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake ParamKey
  deriving anyclass (ToJSONKey, FromJSONKey)

getParamKey :: Param -> ParamKey
getParamKey p = ParamKey
  { name = _paramName p
  , paramIn = _paramIn p
  }
