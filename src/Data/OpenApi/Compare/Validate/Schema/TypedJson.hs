module Data.OpenApi.Compare.Validate.Schema.TypedJson
  ( JsonType (..),
    describeJSONType,
    TypedValue (..),
    untypeValue,
    ForeachType (..),
    foldType,
    forType_,
  )
where

import Algebra.Lattice
import qualified Data.Aeson as A
import Data.Kind
import Data.Monoid
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Typeable

-- | Type of a JSON value
data JsonType
  = Null
  | Boolean
  | Number
  | String
  | Array
  | Object
  deriving stock (Eq, Ord, Show)

describeJSONType :: IsString s => JsonType -> s
describeJSONType = \case
  Null -> "Null"
  Boolean -> "Boolean"
  Number -> "Number"
  String -> "String"
  Array -> "Array"
  Object -> "Object"

-- | A 'A.Value' whose type we know
data TypedValue :: JsonType -> Type where
  TNull :: TypedValue 'Null
  TBool :: !Bool -> TypedValue 'Boolean
  TNumber :: !Scientific -> TypedValue 'Number
  TString :: !Text -> TypedValue 'String
  TArray :: !A.Array -> TypedValue 'Array
  TObject :: !A.Object -> TypedValue 'Object

deriving stock instance Eq (TypedValue t)

deriving stock instance Ord (TypedValue t)

deriving stock instance Show (TypedValue t)

untypeValue :: TypedValue t -> A.Value
untypeValue TNull = A.Null
untypeValue (TBool b) = A.Bool b
untypeValue (TNumber n) = A.Number n
untypeValue (TString s) = A.String s
untypeValue (TArray a) = A.Array a
untypeValue (TObject o) = A.Object o

data ForeachType (f :: JsonType -> Type) = ForeachType
  { forNull :: f 'Null
  , forBoolean :: f 'Boolean
  , forNumber :: f 'Number
  , forString :: f 'String
  , forArray :: f 'Array
  , forObject :: f 'Object
  }

deriving stock instance (forall x. Typeable x => Eq (f x)) => Eq (ForeachType f)

deriving stock instance (forall x. Typeable x => Ord (f x)) => Ord (ForeachType f)

deriving stock instance (forall x. Typeable x => Show (f x)) => Show (ForeachType f)

foldType :: Monoid m => (forall x. Typeable x => JsonType -> (ForeachType f -> f x) -> m) -> m
foldType k =
  k Null forNull
    <> k Boolean forBoolean
    <> k Number forNumber
    <> k String forString
    <> k Array forArray
    <> k Object forObject

forType_ :: Applicative m => (forall x. Typeable x => JsonType -> (ForeachType f -> f x) -> m ()) -> m ()
forType_ k = getAp $ foldType (\ty proj -> Ap $ k ty proj)

broadcastType :: (forall x. Typeable x => f x) -> ForeachType f
broadcastType k =
  ForeachType
    { forNull = k
    , forBoolean = k
    , forNumber = k
    , forString = k
    , forArray = k
    , forObject = k
    }

zipType :: (forall x. Typeable x => f x -> g x -> h x) -> ForeachType f -> ForeachType g -> ForeachType h
zipType k f1 f2 =
  ForeachType
    { forNull = k (forNull f1) (forNull f2)
    , forBoolean = k (forBoolean f1) (forBoolean f2)
    , forNumber = k (forNumber f1) (forNumber f2)
    , forString = k (forString f1) (forString f2)
    , forArray = k (forArray f1) (forArray f2)
    , forObject = k (forObject f1) (forObject f2)
    }

instance (forall x. Lattice (f x)) => Lattice (ForeachType f) where
  (\/) = zipType (\/)
  (/\) = zipType (/\)

instance (forall x. BoundedJoinSemiLattice (f x)) => BoundedJoinSemiLattice (ForeachType f) where
  bottom = broadcastType bottom

instance (forall x. BoundedMeetSemiLattice (f x)) => BoundedMeetSemiLattice (ForeachType f) where
  top = broadcastType top
