module OpenAPI.Checker.Validate.Sums
  ( SumLike (..)
  , checkSums
  ) where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace


data SumLike root a = SumLike
  { value :: a
  , eltStep :: Step root a
  }

checkSums
  :: forall k root t
  .  (Ord k, Subtree root, Steppable root t)
  => (k -> CheckIssue root)
  -> (k -> ProdCons t -> CompatFormula t ())
  -> ProdCons (Map k (SumLike root t))
  -> CompatFormula root ()
checkSums noElt check (ProdCons p c) = for_ (M.toList p) $ \(key, prodElt) ->
  case M.lookup key c of
    Nothing -> issueAt consumer $ noElt key
    Just consElt ->
      let
        sumElts :: ProdCons (SumLike root t)
        sumElts = ProdCons prodElt consElt
        trace = (step . eltStep) <$> sumElts
        elements = value <$> sumElts
      in localTrace trace $ check key elements
