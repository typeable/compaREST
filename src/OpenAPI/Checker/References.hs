module OpenAPI.Checker.References
  ( TracedReferences
  , dereference
  , dereferenceTraced
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
  -> Referenced a
  -> Traced (Referenced a) a
dereference _ (Inline a) = Traced (step InlineStep) a
dereference defs (Ref r@(Reference ref)) =
  Traced (step $ ReferencedStep r) (fromJust $ IOHM.lookup ref defs)

dereferenceTraced
  :: Typeable a
  => Definitions a
  -> Traced r (Referenced a)
  -> Traced r a
dereferenceTraced defs (Traced t x) = retrace t $ dereference defs x
