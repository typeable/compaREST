module Spec.Golden.TraceTree
  ( tests
  )
where

import Control.Category
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.HList
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Paths
import OpenAPI.Checker.PathsPrefixTree
import OpenAPI.Checker.Report
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OpenApi ()
import Spec.Golden.Extra
import Test.Tasty (TestTree, testGroup)
import Text.Pandoc.Class
import Text.Pandoc.Writers
import Prelude hiding (id, (.))

tests :: IO TestTree
tests = do
  traceTreeTests <-
    goldenInputsTreeUniform
      "TraceTree"
      "test/golden/common"
      "trace-tree.yaml"
      ("a.yaml", "b.yaml")
      Yaml.decodeFileThrow
      (pure . BSL.fromStrict . Yaml.encode . runChecker)

  reportTests <-
    goldenInputsTreeUniform
      "Report"
      "test/golden/common"
      "report.md"
      ("a.yaml", "b.yaml")
      Yaml.decodeFileThrow
      (runPandoc . writeMarkdown def . generateReport . runChecker)

  return $ testGroup "Golden tests" [traceTreeTests, reportTests]

runPandoc :: PandocPure Text -> IO BSL.ByteString
runPandoc = either throwIO (pure . BSL.fromStrict . T.encodeUtf8) . runPure

runChecker :: (OpenApi, OpenApi) -> Either (PathsPrefixTree Behave AnIssue 'APILevel) ()
runChecker = runCompatFormula . checkCompatibility HNil Root . toPC
  where
    toPC (client, server) =
      ProdCons
        { producer = traced (step ClientSchema) client
        , consumer = traced (step ServerSchema) server
        }
