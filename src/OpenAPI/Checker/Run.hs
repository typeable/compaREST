module OpenAPI.Checker.Run (runChecker) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.HList
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Options
import OpenAPI.Checker.Paths
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OpenApi ()
import System.Exit
import Prelude hiding (id, (.))

runChecker :: IO ()
runChecker = do
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
  a <- traced Root <$> parseSchema (clientFile opts)
  b <- traced Root <$> parseSchema (serverFile opts)
  let report = runCompatFormula $ checkCompatibility HNil Root (ProdCons a b)
      output = case outputMode opts of
        StdoutMode -> BSC.putStrLn
        FileMode f -> BSC.writeFile f
  case report of
    Right () -> do
      output "No breaking changes found"
      exitSuccess
    Left e -> do
      output $ Yaml.encode e
      exitFailure
