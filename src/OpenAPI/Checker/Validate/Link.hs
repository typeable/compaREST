{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Link () where

import Data.OpenApi
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
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
  describeIssue _ LinksUnsupported = para "OpenApi Diff does not currently support Link Objects."
