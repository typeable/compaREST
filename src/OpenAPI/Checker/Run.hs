module OpenAPI.Checker.Run
  ( runChecker
  , runReport
  , module OpenAPI.Checker.Report
  )
where

import Data.HList
import Data.OpenApi (OpenApi)
import OpenAPI.Checker.Paths
import OpenAPI.Checker.Report
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.OpenApi ()
import Text.Pandoc.Builder

runChecker :: (OpenApi, OpenApi) -> CheckerOutput
runChecker (client, server) =
  CheckerOutput
    { forwardChanges = run client server
    , backwardChanges = run server client
    }
  where
    toPC p c =
      ProdCons
        { producer = traced (step ClientSchema) p
        , consumer = traced (step ServerSchema) c
        }
    run p c = either id mempty . runCompatFormula . checkCompatibility Root HNil $ toPC p c

runReport :: ReportConfig -> (OpenApi, OpenApi) -> (Blocks, ReportStatus)
runReport cfg = generateReport cfg . segregateIssues . runChecker
