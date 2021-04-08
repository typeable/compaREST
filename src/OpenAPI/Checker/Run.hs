module OpenAPI.Checker.Run (runChecker) where

import Control.Category
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.HList
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Options
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OpenApi ()
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
                putStrLn $ show jsonErr
                putStrLn $ show yamlErr
                fail "Exiting"
              Right s -> pure s
          Right s -> pure s
  a <- parseSchema (clientFile opts)
  b <- parseSchema (serverFile opts)
  let report = runCompatFormula (pure id) $ checkCompatibility HNil (ProdCons a b)
  BSC.putStrLn $ Yaml.encode report
