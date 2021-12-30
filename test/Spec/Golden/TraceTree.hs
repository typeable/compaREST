module Spec.Golden.TraceTree
  ( tests
  )
where

import Control.Category
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.OpenApi.Compare.Run
import Data.OpenApi.Compare.Validate.OpenApi ()
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import Paths_compaREST
import Spec.Golden.Extra
import Test.Tasty (TestTree, testGroup)
import Text.Pandoc.Builder
import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Writers
import Prelude hiding (id, (.))

tests :: IO TestTree
tests = do
  dir <- getDataDir
  traceTreeTests <-
    goldenInputsTreeUniform
      "TraceTree"
      dir
      "trace-tree.yaml"
      ("a.yaml", "b.yaml")
      Yaml.decodeFileThrow
      (pure . BSL.fromStrict . Yaml.encode . runChecker)

  reportTests <-
    goldenInputsTreeUniform
      "Report"
      dir
      "report.md"
      ("a.yaml", "b.yaml")
      Yaml.decodeFileThrow
      (runPandoc . writeMarkdown def {writerExtensions = githubMarkdownExtensions} . doc . fst . runReport def)

  return $ testGroup "Golden tests" [traceTreeTests, reportTests]

runPandoc :: PandocPure Text -> IO BSL.ByteString
runPandoc = either throwIO (pure . BSL.fromStrict . T.encodeUtf8) . runPure
