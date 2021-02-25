module OpenAPI.Checker.Validate where

import           Control.Monad
import           Data.Functor
import           Data.HashMap.Strict.InsOrd     as InsMap
import           Data.HashSet.InsOrd            as InsSet
import           Data.OpenApi
import           Data.Traversable
import           OpenAPI.Checker.Report
import           OpenAPI.Checker.Validate.Monad

reportCompat :: OpenApi -> OpenApi -> Report
reportCompat = error "FIXME: reportCompat not implemented"

forwardCompatible :: OpenApi -> OpenApi -> ReportTree
forwardCompatible dec enc = fst $ runReportTreeT $ openApiCompatible dec enc

openApiCompatible :: OpenApi -> OpenApi -> ReportTreeT ()
openApiCompatible dec enc = void $ follow $
  (InsMap.toList $ _openApiPaths dec) <&> \(path, encItem) ->
  (path, checkItem path encItem)
  where
    checkItem path encItem = do
      case (InsMap.lookup path $ _openApiPaths enc) of
        Nothing      -> pathError $ "Path deleted"
        Just decItem -> pathItemsCompatible encItem decItem

pathItemsCompatible :: PathItem -> PathItem -> ReportTreeT ()
pathItemsCompatible = error "FIXME: pathItemsCompatible not implemented"
