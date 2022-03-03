module Main (main) where

import CompaREST.GitHub.API
import CompaREST.GitHub.Action.Config
import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.GitHub
import Control.Monad.Freer.Reader
import Data.OpenApi.Compare.Run
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as Yaml
import qualified GitHub as GH
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
  case sha cfg of
    x | T.null . T.strip . GH.untagName $ x -> do
      putStrLn "SHA not specified. Exiting without doing anything."
      pure ()
    _ ->
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
runPre cfg = runner cfg postStatusProcessing

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

      body = markdown report <> "\n\n" <> footerText cfg

      result =
        if old == new
          then Nothing
          else Just (body, status)
  postStatus result

markdown :: Blocks -> Text
markdown =
  either (error . displayException) id
    . runPure
    . writeHtml5String def
    . doc
