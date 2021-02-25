module OpenAPI.Checker.Run (runChecker) where

import           Data.Aeson
import           Data.OpenApi
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate


runChecker :: IO ()
runChecker = do
  let
    schemaA = (error "FIXME: not implemented")
    schemaB = (error "FIXME: not implemented")
  a <- eitherDecodeFileStrict schemaA >>= \case
    Left e -> do
      putStrLn $ "Failed to parse: " ++ schemaA
      fail e
    Right s -> pure s
  b <- eitherDecodeFileStrict schemaB >>= \case
    Left e -> do
      putStrLn $ "Failed to parse: " ++ schemaB
      fail e
    Right s -> pure s
  let report = reportCompat a b
  printReport report
