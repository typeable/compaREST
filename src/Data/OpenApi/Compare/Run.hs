module Data.OpenApi.Compare.Run
  ( runChecker,
    runReport,
    module Data.OpenApi.Compare.Report,
  )
where

import Data.HList
import Data.OpenApi (OpenApi)
import Data.OpenApi.Compare.Paths
import Data.OpenApi.Compare.Report
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.OpenApi ()
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
