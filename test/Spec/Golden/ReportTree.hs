module Spec.Golden.ReportTree
  ( tests,
  )
where

import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Validate
import Spec.Golden.Extra
import Test.Tasty

tests :: IO TestTree
tests =
  goldenInputsTreeUniform
    "Golden ReportTrees"
    "test/golden/common"
    "report-tree.yaml"
    ("a.yaml", "b.yaml")
    Yaml.decodeFileThrow
    (uncurry forwardCompatible)
