{- | Checks product-like entities. The key is some identificator for the product
element. Each element may be required or optional.

One example of product is request parameters. There are optional and required
parameters. The client and server have possibly different set of
parameters. What we must check is if server requires some request parameter,
then this parameter must be presented by client and their schemas must match.

So when we checking products we are checking from the server's (consumer)
perspective, ensuring that all parameters are provided by the client (producer)
and their schemas match.

This module abstracts this logic for arbitrary elements -}

module OpenAPI.Checker.Validate.Products
  ( checkProducts
  , ProductLike(..)
  ) where

import Data.Foldable
import Data.HList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace

-- | Some entity which is product-like
data ProductLike root a = ProductLike
  { traced :: Traced root a
  , required :: Bool
  }

checkProducts
  :: forall k root t
  .  (Subtree t, Subtree root, Ord k)
  => (k -> CheckIssue root)
  -- ^ No required element found
  -> (k -> ProdCons t -> CompatFormula t ())
  -> ProdCons (Map k (ProductLike root t))
  -> CompatFormula root ()
checkProducts noElt check (ProdCons p c) = for_ (M.toList c) $ \(key, consElt) ->
  case M.lookup key p of
    Nothing -> case required consElt of
      True  -> issueAt producer $ noElt key
      False -> pure ()
    Just prodElt -> do
      let
        elts :: ProdCons (ProductLike root t)
        elts = ProdCons prodElt consElt
        trace = getTrace . traced <$> elts
        elements = getTraced . traced <$> elts
      localTrace trace $ check key elements
