module Main (main) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Maybe
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import FormatHeuristic
import OpenAPI.Checker.Options
import OpenAPI.Checker.Run
import System.Exit
import System.IO
import Text.Pandoc hiding (report)

main :: IO ()
main = do
  opts <- parseOptions
  let parseSchema path =
        eitherDecodeFileStrict path >>= \case
          Left jsonErr -> do
            Yaml.decodeFileEither path >>= \case
              Left yamlErr -> do
                putStrLn "Could not parse as json or yaml"
                print jsonErr
                print yamlErr
                fail "Exiting"
              Right s -> pure s
          Right s -> pure s
  a <- parseSchema (clientFile opts)
  b <- parseSchema (serverFile opts)
  let runPandocIO :: PandocIO a -> ExceptT Errors IO a
      runPandocIO x = lift (runIO x) >>= either (throwError . DocumentError) pure
      options = def {writerExtensions = githubMarkdownExtensions}
      write :: Pandoc -> ExceptT Errors IO ()
      write = case outputMode opts of
        StdoutMode -> lift . T.putStrLn <=< runPandocIO . writeMarkdown options
        FileMode f -> case formatFromFilePath f of
          Nothing -> \_ -> throwError UnknownOutputFormat
          Just (writer, f') -> lift . BSL.writeFile f' <=< runPandocIO . writer
      reportConfig =
        ReportConfig
          { treeStyle = reportTreeStyle opts
          , reportMode = fromMaybe All $ mode opts
          }
      (report, status) = runReport reportConfig (a, b)
  case mode opts of
    Just _ -> either handler pure <=< runExceptT $ write report
    Nothing -> pure ()
  when (signalExitCode opts) $
    case status of
      NoBreakingChanges -> exitSuccess
      BreakingChanges -> exitWith $ ExitFailure 1
      OnlyUnsupportedChanges -> exitWith $ ExitFailure 2

data Errors
  = DocumentError PandocError
  | UnknownOutputFormat

handler :: Errors -> IO a
handler (DocumentError err) = do
  T.hPutStrLn stderr (renderError err)
  exitWith $ ExitFailure 100
handler UnknownOutputFormat = do
  T.hPutStrLn stderr "Could not determine output format from file extension."
  exitWith $ ExitFailure 101
