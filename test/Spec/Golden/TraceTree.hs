module Spec.Golden.TraceTree
  ( tests
  )
where

import Control.Category
import Data.HList
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OpenApi ()
import Spec.Golden.Extra
import Test.Tasty (TestTree)
import Prelude hiding (id, (.))

tests :: IO TestTree
tests =
  goldenInputsTreeUniform
    "Golden TraceTree"
    "test/golden/common"
    "trace-tree.yaml"
    ("a.yaml", "b.yaml")
    Yaml.decodeFileThrow
    (runCompatFormula (pure id) . checkCompatibility HNil . uncurry ProdCons)
