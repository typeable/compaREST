module OpenAPI.Checker.Run (runChecker) where

import Data.Aeson
import OpenAPI.Checker.Options
import OpenAPI.Checker.Report
import OpenAPI.Checker.Validate

runChecker :: IO ()
runChecker = do
  opts <- execParser optionsParserInfo
  let parseSchema path =
        eitherDecodeFileStrict path >>= \case
          Left e -> do
            putStrLn $ "Failed to parse: " ++ path
            fail e
          Right s -> pure s
  a <- parseSchema (clientFile opts)
  b <- parseSchema (serverFile opts)
  let report = reportCompat a b
  printReport report
