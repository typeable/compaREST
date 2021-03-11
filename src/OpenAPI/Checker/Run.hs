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
  (aComponents, a) <- parseSchema (clientFile opts)
  (bComponents, b) <- parseSchema (serverFile opts)
  let -- Contains components from both schemas
      allComponents = aComponents <> bComponents
      report = reportCompat a b
  printReport report
