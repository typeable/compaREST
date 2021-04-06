module OpenAPI.Checker.Validate.Responses
  (
  )
where

import Data.OpenApi
import Data.Foldable
import Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import OpenAPI.Checker.Subtree

instance Subtree Responses where
  type CheckEnv Responses = '[]
  data CheckIssue Responses = ResponseCodeNotFound
    deriving (Eq, Ord, Show)
  -- Here we are checking responses, so, the consumer and producer swap their
  -- roles. The consumer now generates the response and producer consumes
  -- it. So, the logic is swapped.
  checkCompatibility env (ProdCons p c) = do
    for_ (IOHM.toList $  _responsesResponses p) $ \ (prodStatus, prodResponse) ->
      case IOHM.lookup $ _responsesResponses c of
        Nothing -> issueAt consumer ResponseCodeNotFound
        Just consResponse ->
          localStep (ResponseCodeStep prodStatus)
          $ checkCompatibility env
          $ ProdCons prodResponse consResponse
    --  FIXME: Do we need to check "default" fields somehow here?

instance Steppable Responses (Referenced Response) where
  f = undefined
