{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.References
  ( Step (..),
    dereference,
    Typeable,
  )
where

import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Orphans ()
import Data.OpenApi.Compare.Subtree

instance Typeable a => Steppable (Referenced a) a where
  data Step (Referenced a) a = InlineStep
    deriving stock (Eq, Ord, Show)

dereference ::
  Typeable a =>
  Traced (Definitions a) ->
  Traced (Referenced a) ->
  Traced a
dereference defs x = case extract x of
  Inline a ->
    traced (ask x >>> step InlineStep) a
  Ref (Reference ref) ->
    traced (ask defs >>> step (InsOrdHashMapKeyStep ref)) (fromJust $ IOHM.lookup ref $ extract defs)

instance Subtree a => Subtree (Referenced a) where
  type CheckEnv (Referenced a) = ProdCons (Traced (Definitions a)) ': CheckEnv a
  type SubtreeLevel (Referenced a) = SubtreeLevel a

  checkStructuralCompatibility (defs `HCons` env) pc' = do
    let pc = do
          x <- pc'
          defs' <- defs
          pure (dereference defs' x)
    checkSubstructure env pc

  checkSemanticCompatibility (defs `HCons` env) bhv pc' = do
    let pc = do
          x <- pc'
          defs' <- defs
          pure (dereference defs' x)
    checkCompatibility bhv env pc
