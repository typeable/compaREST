module OpenAPI.Checker.Validate.Sums
  ( checkSums
  ) where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Paths

checkSums
  :: (Ord k, Issuable l)
  => Paths q r l
  -> (k -> Issue l)
  -> (k -> ProdCons t -> CompatFormula' q AnIssue r ())
  -> ProdCons (Map k t)
  -> CompatFormula' q AnIssue r ()
checkSums xs noElt check (ProdCons p c) = for_ (M.toList p) $ \(key, prodElt) ->
  case M.lookup key c of
    Nothing -> issueAt xs $ noElt key
    Just consElt ->
      let sumElts = ProdCons prodElt consElt
      in check key sumElts
