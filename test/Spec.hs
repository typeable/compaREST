module Main (main) where

import qualified Spec.Golden.Report
import qualified Spec.Golden.ReportTree
import Test.Tasty

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  goldenReport <- Spec.Golden.Report.tests
  goldenReportTree <- Spec.Golden.ReportTree.tests
  return $
    testGroup
      "Golden tests"
      [ goldenReport,
        goldenReportTree
      ]
