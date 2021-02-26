module OpenAPI.Checker.Aux where

import           Data.OpenApi.Internal

-- | Megre servers from parent (like PathItem) and child (like Operation)
mergeServers :: [Server] -> [Server] -> [Server]
mergeServers parent child = (error "FIXME: not implemented")
