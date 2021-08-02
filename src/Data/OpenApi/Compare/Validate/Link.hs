{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.Link () where

import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Subtree
import Text.Pandoc.Builder

instance Subtree Link where
  type SubtreeLevel Link = 'LinkLevel
  type CheckEnv Link = '[]
  checkStructuralCompatibility _ _ = structuralIssue
  checkSemanticCompatibility _ bhv _ = issueAt bhv LinksUnsupported

instance Issuable 'LinkLevel where
  data Issue 'LinkLevel
    = LinksUnsupported
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    LinksUnsupported -> Unsupported
  describeIssue _ LinksUnsupported = para "CompaREST does not currently support Link Objects."
