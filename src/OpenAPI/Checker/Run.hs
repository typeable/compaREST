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

runChecker :: (OpenApi, OpenApi) -> ReportInput
runChecker (client, server) =
  ReportInput
    { breakingChanges = run client server
    , nonBreakingChanges = run server client
    }
  where
    toPC p c =
      ProdCons
        { producer = traced (step ClientSchema) p
        , consumer = traced (step ServerSchema) c
        }
    run p c = either id mempty . runCompatFormula . checkCompatibility HNil Root $ toPC p c

runReport :: (OpenApi, OpenApi) -> (Pandoc, ReportStatus)
runReport = generateReport . runChecker
