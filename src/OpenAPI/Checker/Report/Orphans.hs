module OpenAPI.Checker.Report.Orphans where

import           Data.HashMap.Strict.InsOrd as InsMap
import           Data.OpenApi.Internal

-- deriving instance Ord Server

-- deriving instance Ord PathItem

-- deriving instance Ord Operation

-- deriving instance Ord Param

deriving instance (Ord a) => Ord (Referenced a)

deriving instance Ord Reference

-- -- Constructors are not exported
-- deriving instance (Ord b) => Ord (InsMap.InsOrdHashMap a b)
