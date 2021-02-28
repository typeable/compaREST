module OpenAPI.Checker.Validate.Dereference where

import           Data.OpenApi.Internal
import           OpenAPI.Checker.Validate.Monad

-- | Throws error if param not found
dereferenceParam :: Referenced Param -> TreeM t Param
dereferenceParam = error "FIXME: dereferenceParam not implemented"
