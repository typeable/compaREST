module OpenAPI.Checker.Aux where

import           Data.OpenApi.Internal

-- | Megre servers from parent (like PathItem) and child (like Operation). Child
-- list may override parent servers list but never remove.
mergeServers :: [Server] -> [Server] -> [Server]
mergeServers parent child = (error "FIXME: not implemented")
