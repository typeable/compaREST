module Main (main) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.HList
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import FormatHeuristic
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Options
import OpenAPI.Checker.Paths
import OpenAPI.Checker.PathsPrefixTree
import OpenAPI.Checker.Report
import OpenAPI.Checker.Subtree
import System.Exit
import System.IO
import Text.Pandoc

main :: IO ()
main = do
  opts <- execParser optionsParserInfo
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
  a <- traced (step ClientSchema) <$> parseSchema (clientFile opts)
  b <- traced (step ServerSchema) <$> parseSchema (serverFile opts)
  let result = runCompatFormula $ checkCompatibility HNil Root (ProdCons a b)
      runPandocIO :: PandocIO a -> ExceptT Errors IO a
      runPandocIO x = lift (runIO x) >>= either (throwError . DocumentError) pure
      output :: Either (PathsPrefixTree Behave AnIssue 'APILevel) () -> ExceptT Errors IO ()
      output = case outputMode opts of
        StdoutMode -> lift . T.putStrLn <=< runPandocIO . writeMarkdown def . generateReport
        FileMode f -> case formatFromFilePath f of
          Nothing -> \_ -> throwError UnknownOutputFormat
          Just (TextWriter writer) -> lift . T.writeFile f <=< runPandocIO . writer def . generateReport
          Just (ByteStringWriter writer) -> lift . BSL.writeFile f <=< runPandocIO . writer def . generateReport
  either handler pure <=< runExceptT $ output result
  case result of
    Right () -> exitSuccess
    Left _ -> exitWith $ ExitFailure 1

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
