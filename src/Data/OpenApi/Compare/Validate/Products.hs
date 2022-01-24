-- | Checks product-like entities. The key is some identificator for the product
--element. Each element may be required or optional.
--
--One example of product is request parameters. There are optional and required
--parameters. The client and server have possibly different set of
--parameters. What we must check is if server requires some request parameter,
--then this parameter must be presented by client and their schemas must match.
--
--So when we checking products we are checking from the server's (consumer)
--perspective, ensuring that all parameters are provided by the client (producer)
--and their schemas match.
--
--This module abstracts this logic for arbitrary elements
module Data.OpenApi.Compare.Validate.Products
  ( checkProducts,
    ProductLike (..),
  )
where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Paths
import Data.OpenApi.Compare.Subtree

-- | Some entity which is product-like
data ProductLike a = ProductLike
  { productValue :: a
  , required :: Bool
  }

checkProducts ::
  (Ord k, Issuable l) =>
  Paths q r l ->
  -- | No required element found
  (k -> Issue l) ->
  (k -> ProdCons t -> CompatFormula' q AnIssue r ()) ->
  ProdCons (Map k (ProductLike t)) ->
  CompatFormula' q AnIssue r ()
checkProducts xs noElt check (ProdCons p c) = for_ (M.toList c) $ \(key, consElt) ->
  case M.lookup key p of
    Nothing -> case required consElt of
      True -> issueAt xs $ noElt key
      False -> pure ()
    Just prodElt -> do
      let elts = ProdCons prodElt consElt
      check key (productValue <$> elts)
