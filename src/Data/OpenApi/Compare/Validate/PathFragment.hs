module Data.OpenApi.Compare.Validate.PathFragment
  ( parsePath,
    PathFragment (..),
    PathFragmentParam,
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Param
import Data.Text (Text)
import qualified Data.Text as T

-- TODO: templates can be only part of the PathFragment. Currently only supports templates as full PathFragment.
-- #23
parsePath :: FilePath -> [PathFragment Text]
parsePath = fmap partition . T.splitOn "/" . T.pack
  where
    partition :: Text -> PathFragment Text
    partition t
      | Just ('{', rest) <- T.uncons t
        , Just (ref, '}') <- T.unsnoc rest =
        DynamicPath ref
    partition t = StaticPath t

-- | Fragment parameterized by parameter. The dynamic part may be either
-- reference to some parameter (in context of operation) or dereferenced
-- parameter itself.
data PathFragment param
  = StaticPath Text
  | DynamicPath param
  deriving stock (Eq, Ord, Show, Functor)

type PathFragmentParam = PathFragment (Traced Param)

instance (Typeable param) => Steppable (PathFragment param) Param where
  data Step (PathFragment param) Param = StaticPathParam Text
    deriving stock (Eq, Ord, Show)

tracedPathFragmentParam :: Traced PathFragmentParam -> Traced Param
tracedPathFragmentParam pfp = case extract pfp of
  StaticPath s ->
    traced (ask pfp >>> step (StaticPathParam s)) $
      mempty
        { _paramRequired = Just True
        , _paramIn = ParamPath
        , _paramAllowEmptyValue = Just False
        , _paramAllowReserved = Just False
        , _paramSchema = Just $ Inline $ staticStringSchema s
        }
  DynamicPath p -> p

staticStringSchema :: Text -> Schema
staticStringSchema t =
  mempty
    { _schemaNullable = Just False
    , _schemaType = Just OpenApiString
    , _schemaEnum = Just [A.String t]
    }

instance Subtree PathFragmentParam where
  type SubtreeLevel PathFragmentParam = 'PathFragmentLevel
  type
    CheckEnv PathFragmentParam =
      '[ProdCons (Traced (Definitions Schema))]

  -- Not much to compare at this level
  checkStructuralCompatibility _ _ = structuralIssue

  -- This case isn't strictly needed. It is here for optimization.
  checkSemanticCompatibility _ beh (ProdCons (extract -> StaticPath x) (extract -> StaticPath y)) =
    if x == y
      then pure ()
      else issueAt beh (PathFragmentsDontMatch (ProdCons x y))
  checkSemanticCompatibility env beh prodCons = do
    checkCompatibility beh env (tracedPathFragmentParam <$> prodCons)
