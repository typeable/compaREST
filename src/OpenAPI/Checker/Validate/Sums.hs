module OpenAPI.Checker.Validate.Sums
  ( checkSums
  ) where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace


checkSums
  :: forall k root t
  .  (Ord k, Subtree root)
  => (k -> CheckIssue root)
  -> (k -> ProdCons t -> CompatFormula t ())
  -> ProdCons (Map k (Traced root t))
  -> CompatFormula root ()
checkSums noElt check (ProdCons p c) = for_ (M.toList p) $ \(key, prodElt) ->
  case M.lookup key c of
    Nothing -> issueAt consumer $ noElt key
    Just consElt ->
      let
        sumElts :: ProdCons (Traced root t)
        sumElts = ProdCons prodElt consElt
        trace = getTrace <$> sumElts
        elements = getTraced <$> sumElts
      in localTrace trace $ check key elements
