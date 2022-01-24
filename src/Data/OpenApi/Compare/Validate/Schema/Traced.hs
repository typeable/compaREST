{-# OPTIONS_GHC -Wno-orphans #-}

module Data.OpenApi.Compare.Validate.Schema.Traced
  ( Step (..),
    tracedAllOf,
    tracedAnyOf,
    tracedOneOf,
    tracedItems,
    tracedAdditionalProperties,
    tracedDiscriminator,
    tracedProperties,
    tracedConjunct,
    PartitionLocation (..),
    PartitionChoice (..),
    Partition,
  )
where

import qualified Data.Aeson as A
import Data.Functor
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List.NonEmpty as NE
import Data.OpenApi
import Data.OpenApi.Compare.Subtree
import qualified Data.Set as S
import Data.Text (Text)

data PartitionChoice
  = CByEnumValue (S.Set A.Value)
  | CByProperties (S.Set Text) (S.Set Text) -- included, excluded
  deriving stock (Eq, Ord, Show)

data PartitionLocation
  = PHere
  | PInProperty Text PartitionLocation
  deriving stock (Eq, Ord, Show)

type Partition = (PartitionLocation, PartitionChoice)

instance Steppable Schema (Referenced Schema) where
  data Step Schema (Referenced Schema)
    = AllOfStep Int
    | OneOfStep Int
    | AnyOfStep Int
    | ItemsObjectStep
    | ItemsArrayStep Int
    | AdditionalPropertiesStep
    | NotStep
    | ImplicitTopSchema
    deriving stock (Eq, Ord, Show)

instance Steppable (Referenced Schema) (Referenced Schema) where
  data Step (Referenced Schema) (Referenced Schema)
    = -- | Invariant (for better memoization only): the "tail" of the trace is
      -- the "least" of the traces of the conjuncted schemata
      ConjunctedWith (NE.NonEmpty (Trace (Referenced Schema)))
    | Partitioned Partition
    deriving stock (Eq, Ord, Show)

instance Steppable Schema (Definitions (Referenced Schema)) where
  data Step Schema (Definitions (Referenced Schema)) = PropertiesStep
    deriving stock (Eq, Ord, Show)

instance Steppable Schema Discriminator where
  data Step Schema Discriminator = DiscriminatorStep
    deriving stock (Eq, Ord, Show)

instance Steppable Discriminator (Definitions (Referenced Schema)) where
  data Step Discriminator (Definitions (Referenced Schema)) = DiscriminatorMapping
    deriving stock (Eq, Ord, Show)

tracedAllOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedAllOf sch =
  _schemaAllOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (AllOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedAnyOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedAnyOf sch =
  _schemaAnyOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (AnyOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedOneOf :: Traced Schema -> Maybe [Traced (Referenced Schema)]
tracedOneOf sch =
  _schemaOneOf (extract sch) <&> \xs ->
    [traced (ask sch >>> step (OneOfStep i)) x | (i, x) <- zip [0 ..] xs]

tracedItems :: Traced Schema -> Maybe (Either (Traced (Referenced Schema)) [Traced (Referenced Schema)])
tracedItems sch =
  _schemaItems (extract sch) <&> \case
    OpenApiItemsObject x -> Left $ traced (ask sch >>> step ItemsObjectStep) x
    OpenApiItemsArray xs ->
      Right
        [traced (ask sch >>> step (ItemsArrayStep i)) x | (i, x) <- zip [0 ..] xs]

tracedAdditionalProperties :: Traced Schema -> Maybe (Either Bool (Traced (Referenced Schema)))
tracedAdditionalProperties sch =
  _schemaAdditionalProperties (extract sch) <&> \case
    AdditionalPropertiesAllowed b -> Left b
    AdditionalPropertiesSchema x -> Right $ traced (ask sch >>> step AdditionalPropertiesStep) x

tracedDiscriminator :: Traced Schema -> Maybe (Traced Discriminator)
tracedDiscriminator = sequence . stepTraced DiscriminatorStep . fmap _schemaDiscriminator

tracedProperties :: Traced Schema -> IOHM.InsOrdHashMap Text (Traced (Referenced Schema))
tracedProperties sch =
  IOHM.mapWithKey
    (\k -> traced (ask sch >>> step PropertiesStep >>> step (InsOrdHashMapKeyStep k)))
    (_schemaProperties $ extract sch)

tracedConjunct :: NE.NonEmpty (Traced (Referenced Schema)) -> Traced (Referenced Schema)
tracedConjunct refSchemas = case NE.sortWith ask refSchemas of
  (rs NE.:| []) -> rs
  (rs1 NE.:| rs2 : rss) ->
    traced (ask rs1 >>> step (ConjunctedWith $ ask <$> rs2 NE.:| rss)) $
      Inline mempty {_schemaAllOf = Just $ extract <$> rs1 : rs2 : rss}
