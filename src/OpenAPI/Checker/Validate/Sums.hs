module OpenAPI.Checker.Validate.Sums
  ( checkSums
  ) where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace


checkSums
  :: forall k r t
  .  (Ord k, Subtree t)
  => (k -> CheckIssue t)
  -> (k -> ProdCons (Traced r t) -> CompatFormula' SubtreeCheckIssue r ())
  -> ProdCons (Map k (Traced r t))
  -> CompatFormula' SubtreeCheckIssue r ()
checkSums noElt check (ProdCons p c) = for_ (M.toList p) $ \(key, prodElt) ->
  case M.lookup key c of
    Nothing -> issueAt prodElt $ noElt key
    Just consElt ->
      let
        sumElts :: ProdCons (Traced r t)
        sumElts = ProdCons prodElt consElt
      in check key sumElts
