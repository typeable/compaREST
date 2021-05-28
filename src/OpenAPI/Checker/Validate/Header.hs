{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Header
  (
  )
where

import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.OpenApi
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.References ()
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Schema ()

instance Subtree Header where
  type SubtreeLevel Header = 'HeaderLevel
  type CheckEnv Header = '[ProdCons (Traced (Definitions Schema))]
  checkStructuralCompatibility env pc = do
    structuralEq $ fmap _headerRequired <$> pc
    structuralEq $ fmap _headerAllowEmptyValue <$> pc
    structuralEq $ fmap _headerExplode <$> pc
    structuralMaybe env $ tracedSchema <$> pc
    pure ()
  checkSemanticCompatibility env beh (ProdCons p c) = do
    if (fromMaybe False $ _headerRequired $ extract c) && not (fromMaybe False $ _headerRequired $ extract p)
      then issueAt beh RequiredHeaderMissing
      else pure ()
    if not (fromMaybe False $ _headerAllowEmptyValue $ extract c) && (fromMaybe False $ _headerAllowEmptyValue $ extract p)
      then issueAt beh NonEmptyHeaderRequired
      else pure ()
    for_ (tracedSchema c) $ \consRef ->
      case tracedSchema p of
        Nothing -> issueAt beh HeaderSchemaRequired
        Just prodRef -> checkCompatibility env (beh >>> step InSchema) (ProdCons prodRef consRef)
    pure ()

instance Steppable Header (Referenced Schema) where
  data Step Header (Referenced Schema) = HeaderSchema
    deriving stock (Eq, Ord, Show)

tracedSchema :: Traced Header -> Maybe (Traced (Referenced Schema))
tracedSchema hdr = _headerSchema (extract hdr) <&> traced (ask hdr >>> step HeaderSchema)

instance Issuable 'HeaderLevel where
  data Issue 'HeaderLevel
    = RequiredHeaderMissing
    | NonEmptyHeaderRequired
    | HeaderSchemaRequired
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False

instance Behavable 'HeaderLevel 'SchemaLevel where
  data Behave 'HeaderLevel 'SchemaLevel
    = InSchema
    deriving stock (Eq, Ord, Show)
  describeBehaviour InSchema = "JSON Schema"
