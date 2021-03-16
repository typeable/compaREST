module OpenAPI.Checker.References
  ( TracedReferences,
    dereferenceTraced,
  )
where

import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Map (Map)
import Data.Maybe
import Data.OpenApi
import Data.Typeable
import OpenAPI.Checker.Orphans
import OpenAPI.Checker.Trace

type TracedReferences root a = Map Reference (Traced root a)

dereferenceTraced ::
  Typeable a =>
  Definitions a ->
  Referenced a ->
  Traced (Referenced a) a
dereferenceTraced _ (Inline a) = Traced (step InlineStep) a
dereferenceTraced defs (Ref r@(Reference ref)) =
  Traced (step $ ReferencedStep r) (fromJust $ IOHM.lookup ref defs)
