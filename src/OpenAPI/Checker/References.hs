module OpenAPI.Checker.References
  ( TracedReferences
  , dereference
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

dereference
  :: Typeable a
  => Definitions a
  -> Traced r (Referenced a)
  -> Traced r a
dereference defs x = case extract x of
  Inline a
    -> traced (ask x >>> step InlineStep) a
  Ref r@(Reference ref)
    -> traced (ask x >>> step (ReferencedStep r)) (fromJust $ IOHM.lookup ref defs)
