{-# OPTIONS_GHC -Wno-orphans #-}
module OpenAPI.Checker.References
  ( Step (..)
  , dereference
  )
where

import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import Data.Typeable
import OpenAPI.Checker.Orphans ()
import OpenAPI.Checker.Subtree

instance Typeable a => Steppable (Referenced a) a where
  data Step (Referenced a) a = InlineStep
    deriving stock (Eq, Ord, Show)

instance Typeable a => Steppable (Definitions a) a where
  data Step (Definitions a) a = ReferencedStep Reference
    deriving stock (Eq, Ord, Show)

dereference
  :: Typeable a
  => Traced (Definitions a)
  -> Traced (Referenced a)
  -> Traced a
dereference defs x = case extract x of
  Inline a
    -> traced (ask x >>> step InlineStep) a
  Ref r@(Reference ref)
    -> traced (ask defs >>> step (ReferencedStep r)) (fromJust $ IOHM.lookup ref $ extract defs)
