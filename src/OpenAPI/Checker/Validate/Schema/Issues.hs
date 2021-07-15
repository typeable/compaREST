{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.Schema.Issues
  ( Issue (..)
  , Behave (..)
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import OpenAPI.Checker.Behavior
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Validate.Schema.JsonFormula
import OpenAPI.Checker.Validate.Schema.Partition
import OpenAPI.Checker.Validate.Schema.TypedJson
import Text.Pandoc.Builder hiding (Format, Null)

instance Issuable 'TypedSchemaLevel where
  data Issue 'TypedSchemaLevel
    = -- | producer produces a specific value ($1), consumer has a condition that is not satisfied by said value
      EnumDoesntSatisfy A.Value
    | -- | consumer only expects a specific value which the producer does not produce.
      NoMatchingEnum A.Value
    | -- | consumer declares a maximum numeric value ($1), producer doesn't
      NoMatchingMaximum (Bound Scientific)
    | -- | consumer declares a maximum numeric value ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaximumWeak (ProdCons (Bound Scientific))
    | -- | consumer declares a minimum numeric value, producer doesn't
      NoMatchingMinimum (Bound Scientific)
    | -- | consumer declares a minimum numeric value ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinimumWeak (ProdCons (Bound Scientific))
    | -- | consumer declares that the numeric value must be a multiple of $1, producer doesn't
      NoMatchingMultipleOf Scientific
    | -- | consumer declares that the numeric value must be a multiple of $1, producer declares a weaker condition (multiple of $2)
      MatchingMultipleOfWeak (ProdCons Scientific)
    | -- | consumer declares a string/number format, producer declares none or a different format (TODO: improve via regex #32)
      NoMatchingFormat Format
    | -- | consumer declares a maximum length of the string ($1), producer doesn't.
      NoMatchingMaxLength Integer
    | -- | consumer declares a maximum length of the string ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxLengthWeak (ProdCons Integer)
    | -- | consumer declares a minimum length of the string ($1), producer doesn't.
      NoMatchingMinLength Integer
    | -- | consumer declares a minimum length of the string ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinLengthWeak (ProdCons Integer)
    | -- | consumer declares the string value must match a regex ($1), producer doesn't declare or declares different regex (TODO: #32)
      NoMatchingPattern Pattern
    | -- | consumer declares the items of an array must satisfy some condition, producer doesn't
      NoMatchingItems
    | -- | consumer declares a maximum length of the array ($1), producer doesn't.
      NoMatchingMaxItems Integer
    | -- | consumer declares a maximum length of the array ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxItemsWeak (ProdCons Integer)
    | -- | consumer declares a minimum length of the array ($1), producer doesn't.
      NoMatchingMinItems Integer
    | -- | consumer declares a minimum length of the array ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinItemsWeak (ProdCons Integer)
    | -- | consumer declares that items must be unique, producer doesn't
      NoMatchingUniqueItems
    | -- | consumer declares the properties of an object must satisfy some condition, producer doesn't
      NoMatchingProperties
    | -- | producer allows additional properties, consumer doesn't
      NoAdditionalProperties
    | -- | consumer declares a maximum number of properties in the object ($1), producer doesn't.
      NoMatchingMaxProperties Integer
    | -- | consumer declares a maximum number of properties in the object ($1), producer declares a weaker (higher) limit ($2)
      MatchingMaxPropertiesWeak (ProdCons Integer)
    | -- | consumer declares a minimum number of properties in the object ($1), producer doesn't.
      NoMatchingMinProperties Integer
    | -- | consumer declares a minimum number of properties in the object ($1), producer declares a weaker (lower) limit ($2)
      MatchingMinPropertiesWeak (ProdCons Integer)
    | -- | producer declares that the value must satisfy a disjunction of some conditions, but consumer's requirements couldn't be matched against any single one of them (TODO: split heuristic #71)
      NoMatchingCondition (Maybe Partition) [SomeCondition]
    | -- | producer indicates that values of this type are now allowed, but the consumer does not do so (currently we only check immediate contradictions, c.f. #70)
      -- AKA consumer does not have the type
      NoContradiction
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported _ = False
  describeIssue Forward (EnumDoesntSatisfy v) = para "The following enum value was removed:" <> showJSONValue v
  describeIssue Backward (EnumDoesntSatisfy v) = para "The following enum value was added:" <> showJSONValue v
  describeIssue Forward (NoMatchingEnum v) = para "The following enum value has been added:" <> showJSONValue v
  describeIssue Backward (NoMatchingEnum v) = para "The following enum value has been removed:" <> showJSONValue v
  describeIssue Forward (NoMatchingMaximum b) = para $ "Upper bound has been added:" <> showBound b <> "."
  describeIssue Backward (NoMatchingMaximum b) = para $ "Upper bound has been removed:" <> showBound b <> "."
  describeIssue _ (MatchingMaximumWeak (ProdCons p c)) = para $ "Upper bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMinimum b) = para $ "Lower bound has been added: " <> showBound b <> "."
  describeIssue Backward (NoMatchingMinimum b) = para $ "Lower bound has been removed: " <> showBound b <> "."
  describeIssue _ (MatchingMinimumWeak (ProdCons p c)) = para $ "Lower bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMultipleOf n) = para $ "Value is now a multiple of " <> show' n <> "."
  describeIssue Backward (NoMatchingMultipleOf n) = para $ "Value is no longer a multiple of " <> show' n <> "."
  describeIssue _ (MatchingMultipleOfWeak (ProdCons p c)) = para $ "Value changed from being a multiple of " <> show' p <> " to being a multiple of " <> show' c <> "."
  describeIssue Forward (NoMatchingFormat f) = para $ "Format added: " <> code f <> "."
  describeIssue Backward (NoMatchingFormat f) = para $ "Format removed: " <> code f <> "."
  describeIssue Forward (NoMatchingMaxLength n) = para $ "Maximum length added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxLength n) = para $ "Maximum length removed: " <> show' n <> "."
  describeIssue _ (MatchingMaxLengthWeak (ProdCons p c)) = para $ "Maximum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinLength n) = para $ "Minimum length of the string added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinLength n) = para $ "Minimum length of the string removed: " <> show' n <> "."
  describeIssue _ (MatchingMinLengthWeak (ProdCons p c)) = para $ "Minimum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingPattern p) = para "Pattern (regular expression) added: " <> codeBlock p
  describeIssue Backward (NoMatchingPattern p) = para "Pattern (regular expression) removed: " <> codeBlock p
  describeIssue Forward NoMatchingItems = para "Array item schema has been added."
  describeIssue Backward NoMatchingItems = para "Array item schema has been removed."
  describeIssue Forward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been added " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been removed " <> show' n <> "."
  describeIssue _ (MatchingMaxItemsWeak (ProdCons p c)) = para $ "Maximum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinItems n) = para $ "Minimum length of the array added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinItems n) = para $ "Minimum length of the array removed: " <> show' n <> "."
  describeIssue _ (MatchingMinItemsWeak (ProdCons p c)) = para $ "Minimum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward NoMatchingUniqueItems = para "Items are now required to be unique."
  describeIssue Backward NoMatchingUniqueItems = para "Items are no longer required to be unique."
  describeIssue Forward NoMatchingProperties = para "Property added."
  describeIssue Backward NoMatchingProperties = para "Property removed."
  describeIssue Forward NoAdditionalProperties = para "Additional properties have been removed."
  describeIssue Backward NoAdditionalProperties = para "Additional properties have been added."
  describeIssue Forward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been removed: " <> show' n <> "."
  describeIssue _ (MatchingMaxPropertiesWeak (ProdCons p c)) = para $ "Maximum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinProperties n) = para $ "Minimum number of properties added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinProperties n) = para $ "Minimum number of properties removed: " <> show' n <> "."
  describeIssue _ (MatchingMinPropertiesWeak (ProdCons p c)) = para $ "Minimum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue _ (NoMatchingCondition mPart conds) =
    para
      (case mPart of
         Nothing -> "Could not verify that the following conditions hold (please file a bug if you see this)"
         Just locPart ->
           showPartition locPart
             <> " – could not verify that the following conditions hold (please file a bug if you see this):")
      <> bulletList ((\(SomeCondition c) -> showCondition c) <$> conds)
  describeIssue Forward NoContradiction = para "The value has been removed."
  describeIssue Backward NoContradiction = para "The value has been added."

show' :: Show x => x -> Inlines
show' = str . T.pack . show

instance Issuable 'SchemaLevel where
  data Issue 'SchemaLevel
    = -- | Some (openapi-supported) feature that we do not support was encountered in the schema
      NotSupported Text
    | -- | We couldn't prove that the branches of a oneOf are disjoint, and we will treat it as an anyOf, meaning we don't check whether the overlaps are excluded in a compatible way
      OneOfNotDisjoint
    | -- | The schema is actually invalid
      InvalidSchema Text
    | -- | The schema contains a reference loop along "anyOf"/"allOf"/"oneOf".
      UnguardedRecursion
    | -- | Producer doesn't place any restrictions on the types, but the consumer does. List what types remain available in the consumer.
      TypesRestricted [JsonType]
    | -- | in the producer this field used to be handled as part of "additionalProperties", and the consumer this is a specific "properties" entry. Only thrown when this change actually causes other issues
      AdditionalToProperty
    | -- | in the consumer this field used to be handled as part of "additionalProperties", and the producer this is a specific "properties" entry. Only thrown when this change actually causes other issues
      PropertyToAdditional
    | -- | consumer requires a property that is not required/allowed in the producer
      PropertyNowRequired
    | -- | producer allows a property that is not allowed in the consumer
      UnexpectedProperty
    deriving stock (Eq, Ord, Show)
  issueIsUnsupported = \case
    NotSupported _ -> True
    OneOfNotDisjoint -> True
    InvalidSchema _ -> True
    UnguardedRecursion -> True
    _ -> False

  describeIssue _ (NotSupported i) =
    para (emph "Encountered a feature that OpenApi Diff does not support: " <> text i <> ".")
  describeIssue _ OneOfNotDisjoint =
    para (emph "Treating oneOf as anyOf (couldn't check overlaps)")
  describeIssue _ (InvalidSchema i) =
    para (emph "The schema is invalid: " <> text i <> ".")
  describeIssue _ UnguardedRecursion =
    para "Encountered recursion that is too complex for OpenApi Diff to untangle."
  describeIssue Forward (TypesRestricted tys) = case tys of
    [] -> para "No longer has any valid values." -- weird
    _ -> para "Values are now limited to the following types: " <> bulletList (para . describeJSONType <$> tys)
  describeIssue Backward (TypesRestricted tys) = case tys of
    [] -> para "Any value of any type is now allowed." -- weird
    _ -> para "Values are no longer limited to the following types: " <> bulletList (para . describeJSONType <$> tys)
  describeIssue Forward AdditionalToProperty = para "The property was previously implicitly described by the catch-all \"additional properties\" case. It is now explicitly defined."
  describeIssue Backward AdditionalToProperty = para "The property was previously explicitly defined. It is now implicitly described by the catch-all \"additional properties\" case."
  describeIssue Forward PropertyToAdditional = para "The property was previously explicitly defined. It is now implicitly described by the catch-all \"additional properties\" case."
  describeIssue Backward PropertyToAdditional = para "The property was previously implicitly described by the catch-all \"additional properties\" case. It is now explicitly defined."
  describeIssue Forward PropertyNowRequired = para "The property has become required."
  describeIssue Backward PropertyNowRequired = para "The property may not be present."
  describeIssue Forward UnexpectedProperty = para "The property has been removed."
  describeIssue Backward UnexpectedProperty = para "The property has been added."

instance Behavable 'SchemaLevel 'TypedSchemaLevel where
  data Behave 'SchemaLevel 'TypedSchemaLevel
    = OfType JsonType
    deriving stock (Eq, Ord, Show)

  describeBehaviour (OfType t) = describeJSONType t

instance Behavable 'TypedSchemaLevel 'TypedSchemaLevel where
  data Behave 'TypedSchemaLevel 'TypedSchemaLevel
    = InPartition Partition
    deriving stock (Eq, Ord, Show)

  describeBehaviour (InPartition partition) = showPartition partition

instance Behavable 'TypedSchemaLevel 'SchemaLevel where
  data Behave 'TypedSchemaLevel 'SchemaLevel
    = InItems
    | InProperty Text
    | InAdditionalProperty
    deriving stock (Eq, Ord, Show)

  describeBehaviour InItems = "Items"
  describeBehaviour (InProperty p) = "Property " <> code p
  describeBehaviour InAdditionalProperty = "Additional properties"
