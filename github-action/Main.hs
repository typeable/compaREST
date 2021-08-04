module Main (main) where

import CompaREST.GitHub.API
import CompaREST.GitHub.Action.Config
import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.GitHub
import Control.Monad.Freer.Reader
import Data.Text (Text)
import qualified Data.Yaml.Aeson as Yaml
import qualified GitHub as GH
import OpenAPI.Checker.Run
import System.Environment
import System.Envy (decodeEnv)
import System.FilePath ((</>))
import Text.Pandoc (runPure)
import Text.Pandoc.Builder
import Text.Pandoc.Options
import Text.Pandoc.Writers

main :: IO ()
main = do
  cfg <- decodeEnv >>= either error pure
  getArgs >>= \case
    ["pre"] -> runPre cfg
    ["run"] -> do
      oldFile <- getEnv "OLD"
      newFile <- getEnv "NEW"
      runRun cfg (root cfg </> oldFile) (root cfg </> newFile)
    _ -> error "Invalid arguments."

runner :: Config -> Eff '[GitHub, Error GH.Error, Reader Config, IO] a -> IO a
runner cfg =
  runM @IO . runReader cfg
    . flip (handleError @GH.Error) (error . displayException)
    . runGitHub (githubToken cfg)

runPre :: Config -> IO ()
runPre cfg =
  runner cfg $
    mapComment
      (markdown (header 4 "⏳ Report might not be accurate. Attempting to update." <> horizontalRule) <>)

runRun :: Config -> FilePath -> FilePath -> IO ()
runRun cfg old' new' = runner cfg $ do
  old <- Yaml.decodeFileThrow old'
  new <- Yaml.decodeFileThrow new'
  let reportConfig =
        ReportConfig
          { treeStyle = FoldingBlockquotesTreeStyle
          , reportMode = All
          }
      (report, status) = runReport reportConfig (old, new)

      summaryDetail s d =
        rawHtml "<details>"
          <> rawHtml "<summary>"
          <> s
          <> rawHtml "</summary>"
          <> d
          <> rawHtml "</details>"
        where
          rawHtml = rawBlock "html"

      message =
        header 3 (text $ "⛄ compaREST – " <> projectName cfg)
          <> if old == new
            then header 1 "✅ The API did not change"
            else
              header
                1
                ( case status of
                    BreakingChanges -> "⚠️ Breaking changes found!"
                    NoBreakingChanges -> "No breaking changes found ✨"
                    OnlyUnsupportedChanges -> "🤷 Couldn't determine compatibility"
                )
                <> summaryDetail (plain "ℹ️ Details") report
      messageBody = markdown message <> "\n\n" <> footerText cfg
  createOrUpdateComment messageBody

markdown :: Blocks -> Text
markdown =
  either (error . displayException) id
    . runPure
    . writeHtml5String def
    . doc
