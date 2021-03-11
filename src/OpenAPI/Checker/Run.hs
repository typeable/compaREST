module OpenAPI.Checker.Run (runChecker) where

import Data.Aeson
import Data.OpenApi
import OpenAPI.Checker.Options
import OpenAPI.Checker.Report
import OpenAPI.Checker.Validate
import OpenAPI.Checker.Validate.Dereference

runChecker :: IO ()
runChecker = do
  opts <- execParser optionsParserInfo
  let parseSchema path =
        eitherDecodeFileStrict path >>= \case
          Left e -> do
            putStrLn $ "Failed to parse: " ++ path
            fail e
          Right s -> uniqRefs (_openApiComponents s) s
  (_, a) <- parseSchema (clientFile opts)
  (_, b) <- parseSchema (serverFile opts)
  let report = reportCompat a b
  printReport report
