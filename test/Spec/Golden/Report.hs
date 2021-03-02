module Spec.Golden.Report
  ( tests,
  )
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Validate
import Spec.Golden.Extra
import Test.Tasty

tests :: IO TestTree
tests =
  goldenInputsTreeUniform
    "Golden Reports"
    "test/golden/common"
    "report.yaml"
    ("a.yaml", "b.yaml")
    Yaml.decodeFileThrow
    (\(a, b) -> BSL.fromStrict . Yaml.encode $ reportCompat a b)
