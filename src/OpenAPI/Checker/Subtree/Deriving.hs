module OpenAPI.Checker.Subtree.Deriving (EqSubtree (..)) where

import Control.Monad
import OpenAPI.Checker.Subtree

newtype EqSubtree t = EqSubtree t
