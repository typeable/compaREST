module OpenAPI.Checker.Report
  ( generateReport
  )
where

import qualified Data.Map as M
import Data.TypeRepMap
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.PathsPrefixTree
import qualified OpenAPI.Checker.PathsPrefixTree as P
import Text.Pandoc.Builder.Extra

generateReport :: Either (P.PathsPrefixTree Behave AnIssue 'APILevel) () -> Pandoc
generateReport (Right ()) = doc $ header1 "No breaking changes found ✨"
generateReport (Left errs) = doc $ showErrs errs

showErrs :: P.PathsPrefixTree Behave AnIssue a -> Blocks
showErrs (P.PathsPrefixNode currentIssues subIssues) =
  foldMap (\(AnIssue i) -> describeIssue i) currentIssues
    <> foldMap
      (\(WrapTypeable (AStep m)) ->
         M.foldMapWithKey
           (\bhv subErrors ->
              header1 (describeBehaviour bhv)
                <> sub (showErrs subErrors))
           m)
      subIssues
