{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.References
  ( Step (..)
  , dereference
  )
where

import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import qualified Data.OpenApi.Schema.Generator as G
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
  Inline a ->
    traced (ask x >>> step InlineStep) a
  Ref r@(Reference ref) ->
    traced (ask defs >>> step (ReferencedStep r)) (fromJust $ IOHM.lookup ref $ extract defs)

instance Subtree a => Subtree (Referenced a) where
  type CheckEnv (Referenced a) = ProdCons (Traced (Definitions a)) ': CheckEnv a
  type SubtreeLevel (Referenced a) = SubtreeLevel a

  checkStructuralCompatibility env pc' = do
    let pc = do
          x <- pc'
          defs <- extract <$> getH @(ProdCons (Traced (Definitions a))) env
          pure (G.dereference defs x)
    checkStructuralCompatibility env pc

  checkSemanticCompatibility env bhv pc' = do
    let pc = do
          x <- pc'
          defs <- getH @(ProdCons (Traced (Definitions a))) env
          pure (dereference defs x)
    checkCompatibility env bhv pc
