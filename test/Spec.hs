module Main (main) where

import qualified Spec.Golden.TraceTree
import Test.Tasty

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  goldenReportTree <- Spec.Golden.TraceTree.tests
  return . localOption (mkTimeout 1000000) $ goldenReportTree
