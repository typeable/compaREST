{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.Schema.Issues
  ( Issue (..),
    Behave (..),
  )
where

import qualified Data.Aeson as A
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.Schema.JsonFormula
import Data.OpenApi.Compare.Validate.Schema.Partition
import Data.OpenApi.Compare.Validate.Schema.TypedJson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
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
    | -- | producer and consumer declare that an array must be a tuple of a fixed length, but the lengths don't match
      TupleItemsLengthChanged (ProdCons Integer)
    | -- | consumer declares that the array is a tuple, but the producer doesn't, the length constraints match, but there were issues with the components
      ArrayToTuple
    | -- | producer declares that the array is a tuple, but the consumer doesn't, and there were issues with the components
      TupleToArray
    | -- | consumer declares that the array is a tuple, but the producer doesn't, and there aren't sufficient length constraints
      NoMatchingTupleItems
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
    | -- | consumer indicates that no values of this type are allowed, but we weren't able to conclude that in the producer (currently only immediate contradictions are checked, c.f. #70)
      TypeBecomesEmpty
    | -- | consumer indicates that no values in a particular partition are allowed, but we weren't able to conclude this in the producer
      PartitionBecomesEmpty Partition
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    NoMatchingEnum _ -> ProbablyIssue
    MatchingMaximumWeak _ -> ProbablyIssue -- interplay with MultipleOf could make this not an issue
    MatchingMinimumWeak _ -> ProbablyIssue -- ditto
    MatchingMultipleOfWeak _ -> ProbablyIssue -- ditto
    NoMatchingFormat _ -> Unsupported
    NoMatchingPattern _ -> Unsupported
    ArrayToTuple -> Comment
    TupleToArray -> Comment
    NoMatchingProperties -> ProbablyIssue -- TODO: #109
    TypeBecomesEmpty -> ProbablyIssue -- TODO: #70
    PartitionBecomesEmpty _ -> ProbablyIssue -- ditto
    _ -> CertainIssue
  relatedIssues =
    (==) `withClass` \case
      EnumDoesntSatisfy v -> Just v
      NoMatchingEnum v -> Just v
      _ -> Nothing
      `withClass` \case
        NoMatchingMaximum _ -> Just ()
        MatchingMaximumWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMinimum _ -> Just ()
        MatchingMinimumWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMultipleOf _ -> Just ()
        MatchingMultipleOfWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMaxLength _ -> Just ()
        MatchingMaxLengthWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMinLength _ -> Just ()
        MatchingMinLengthWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingItems -> Just ()
        ArrayToTuple -> Just ()
        TupleToArray -> Just ()
        NoMatchingTupleItems -> Just ()
        TupleItemsLengthChanged _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMaxItems _ -> Just ()
        MatchingMaxItemsWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMinItems _ -> Just ()
        MatchingMinItemsWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMaxProperties _ -> Just ()
        MatchingMaxPropertiesWeak _ -> Just ()
        _ -> Nothing
      `withClass` \case
        NoMatchingMinProperties _ -> Just ()
        MatchingMinPropertiesWeak _ -> Just ()
        _ -> Nothing
  describeIssue Forward (EnumDoesntSatisfy v) = para "The following enum value was removed:" <> showJSONValue v
  describeIssue Backward (EnumDoesntSatisfy v) = para "The following enum value was added:" <> showJSONValue v
  describeIssue Forward (NoMatchingEnum v) = para "The following enum value has been added:" <> showJSONValue v
  describeIssue Backward (NoMatchingEnum v) = para "The following enum value has been removed:" <> showJSONValue v
  describeIssue Forward (NoMatchingMaximum b) = para $ "Upper bound has been added:" <> showBound b <> "."
  describeIssue Backward (NoMatchingMaximum b) = para $ "Upper bound has been removed:" <> showBound b <> "."
  describeIssue ori (MatchingMaximumWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Upper bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMinimum b) = para $ "Lower bound has been added: " <> showBound b <> "."
  describeIssue Backward (NoMatchingMinimum b) = para $ "Lower bound has been removed: " <> showBound b <> "."
  describeIssue ori (MatchingMinimumWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Lower bound changed from " <> showBound p <> " to " <> showBound c <> "."
  describeIssue Forward (NoMatchingMultipleOf n) = para $ "Value is now a multiple of " <> show' n <> "."
  describeIssue Backward (NoMatchingMultipleOf n) = para $ "Value is no longer a multiple of " <> show' n <> "."
  describeIssue ori (MatchingMultipleOfWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Value changed from being a multiple of " <> show' p <> " to being a multiple of " <> show' c <> "."
  describeIssue Forward (NoMatchingFormat f) = para $ "Format added: " <> code f <> "."
  describeIssue Backward (NoMatchingFormat f) = para $ "Format removed: " <> code f <> "."
  describeIssue Forward (NoMatchingMaxLength n) = para $ "Maximum length added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxLength n) = para $ "Maximum length removed: " <> show' n <> "."
  describeIssue ori (MatchingMaxLengthWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Maximum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinLength n) = para $ "Minimum length of the string added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinLength n) = para $ "Minimum length of the string removed: " <> show' n <> "."
  describeIssue ori (MatchingMinLengthWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Minimum length of the string changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingPattern p) = para "Pattern (regular expression) added: " <> codeBlock p
  describeIssue Backward (NoMatchingPattern p) = para "Pattern (regular expression) removed: " <> codeBlock p
  describeIssue Forward NoMatchingItems = para "Array item schema has been added."
  describeIssue Backward NoMatchingItems = para "Array item schema has been removed."
  describeIssue ori (TupleItemsLengthChanged (orientProdCons ori -> ProdCons p c)) =
    para $ "Tuple length changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward ArrayToTuple = para "The array is now explicitly defined as a tuple."
  describeIssue Backward ArrayToTuple = para "The array is no longer explicitly defined as a tuple."
  describeIssue Forward TupleToArray = para "The array is no longer explicitly defined as a tuple."
  describeIssue Backward TupleToArray = para "The array is now explicitly defined as a tuple."
  describeIssue Forward NoMatchingTupleItems = para "The array is now explicitly defined as a tuple."
  describeIssue Backward NoMatchingTupleItems = para "The array is no longer explicitly defined as a tuple."
  describeIssue Forward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been added " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxItems n) = para $ "Maximum length of the array has been removed " <> show' n <> "."
  describeIssue ori (MatchingMaxItemsWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Maximum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinItems n) = para $ "Minimum length of the array added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinItems n) = para $ "Minimum length of the array removed: " <> show' n <> "."
  describeIssue ori (MatchingMinItemsWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Minimum length of the array changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward NoMatchingUniqueItems = para "Items are now required to be unique."
  describeIssue Backward NoMatchingUniqueItems = para "Items are no longer required to be unique."
  describeIssue Forward NoMatchingProperties = para "Property added."
  describeIssue Backward NoMatchingProperties = para "Property removed."
  describeIssue Forward NoAdditionalProperties = para "Additional properties have been removed."
  describeIssue Backward NoAdditionalProperties = para "Additional properties have been added."
  describeIssue Forward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMaxProperties n) = para $ "Maximum number of properties has been removed: " <> show' n <> "."
  describeIssue ori (MatchingMaxPropertiesWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Maximum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue Forward (NoMatchingMinProperties n) = para $ "Minimum number of properties added: " <> show' n <> "."
  describeIssue Backward (NoMatchingMinProperties n) = para $ "Minimum number of properties removed: " <> show' n <> "."
  describeIssue ori (MatchingMinPropertiesWeak (orientProdCons ori -> ProdCons p c)) =
    para $ "Minimum number of properties has changed from " <> show' p <> " to " <> show' c <> "."
  describeIssue _ (NoMatchingCondition mPart conds) =
    para
      ( case mPart of
          Nothing -> "Could not verify that the following conditions hold (please file a bug if you see this):"
          Just locPart ->
            "In cases where " <> showPartition locPart
              <> " – could not verify that the following conditions hold (please file a bug if you see this):"
      )
      <> bulletList ((\(SomeCondition c) -> showCondition c) <$> conds)
  describeIssue Forward TypeBecomesEmpty = para "The type has been removed."
  describeIssue Backward TypeBecomesEmpty = para "The type has been added."
  describeIssue Forward (PartitionBecomesEmpty part) = para $ "The case where " <> showPartition part <> " – has been removed."
  describeIssue Backward (PartitionBecomesEmpty part) = para $ "The case where " <> showPartition part <> " – has been added."

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
  issueKind = \case
    NotSupported _ -> Unsupported
    OneOfNotDisjoint -> Unsupported
    InvalidSchema _ -> SchemaInvalid
    UnguardedRecursion -> Unsupported
    AdditionalToProperty -> Comment
    PropertyToAdditional -> Comment
    TypesRestricted _ -> ProbablyIssue -- TODO: #70
    _ -> CertainIssue
  relatedIssues =
    (==) `withClass` \case
      AdditionalToProperty -> Just ()
      PropertyToAdditional -> Just ()
      _ -> Nothing
      `withClass` \case
        PropertyNowRequired -> Just ()
        UnexpectedProperty -> Just ()
        _ -> Nothing
  describeIssue _ (NotSupported i) =
    para (emph "Encountered a feature that CompaREST does not support: " <> text i <> ".")
  describeIssue _ OneOfNotDisjoint =
    para $
      "Could not deduce that " <> code "oneOf"
        <> " cases don't overlap. Treating the "
        <> code "oneOf"
        <> " as an "
        <> code "anyOf"
        <> ". Reported errors might not be accurate."
  describeIssue _ (InvalidSchema i) =
    para (emph "The schema is invalid: " <> text i <> ".")
  describeIssue _ UnguardedRecursion =
    para "Encountered recursion that is too complex for CompaREST to untangle."
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

  describeBehavior (OfType t) = describeJSONType t

instance Behavable 'TypedSchemaLevel 'TypedSchemaLevel where
  data Behave 'TypedSchemaLevel 'TypedSchemaLevel
    = InPartition Partition
    deriving stock (Eq, Ord, Show)

  describeBehavior (InPartition partition) = "In cases where " <> showPartition partition

instance Behavable 'TypedSchemaLevel 'SchemaLevel where
  data Behave 'TypedSchemaLevel 'SchemaLevel
    = InItems
    | InItem Integer
    | InProperty Text
    | InAdditionalProperty
    deriving stock (Eq, Ord, Show)

  describeBehavior InItems = "Items"
  describeBehavior (InItem i) = "Item " <> show' i
  describeBehavior (InProperty p) = "Property " <> code p
  describeBehavior InAdditionalProperty = "Additional properties"
