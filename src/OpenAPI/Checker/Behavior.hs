module OpenAPI.Checker.Behavior
  ( BehaviorLevel (..)
  , Behavable (..)
  , IssueKind (..)
  , Issuable (..)
  , Orientation (..)
  , toggleOrientation
  , Behavior
  , AnIssue (..)
  )
where

import Data.Aeson
import Data.Kind
import Data.Typeable
import OpenAPI.Checker.Paths
import Text.Pandoc.Builder

-- | Kind
data BehaviorLevel
  = APILevel
  | ServerLevel
  | SecurityRequirementLevel
  | SecuritySchemeLevel
  | PathLevel
  | OperationLevel
  | PathFragmentLevel
  | RequestLevel
  | ResponseLevel
  | HeaderLevel
  | -- | either request or response data
    PayloadLevel
  | SchemaLevel
  | TypedSchemaLevel
  | LinkLevel
  | CallbackLevel

class
  (Ord (Behave a b), Show (Behave a b)) =>
  Behavable (a :: BehaviorLevel) (b :: BehaviorLevel)
  where
  data Behave a b
  describeBehavior :: Behave a b -> Inlines

type instance AdditionalQuiverConstraints Behave a b = Behavable a b

data IssueKind
  = -- | This is certainly an issue, we can demonstrate a "counterexample"
    CertainIssue
  | -- | Change looks breaking but we don't have a complete comprehension of the problem
    ProbablyIssue
  | -- | We don't really support this feature at all, outside structural comparison
    Unsupported
  | -- | This is not an issue in itself, but a clarifying comment providing context for some other issues
    Comment
  | -- | We detected an issue with one of the input schemata itself
    SchemaInvalid
  deriving stock (Eq, Ord, Show)

class (Typeable l, Ord (Issue l), Show (Issue l)) => Issuable (l :: BehaviorLevel) where
  data Issue l :: Type

  -- | The same issues can be rendered in multiple places and might
  -- require different ways of represnting them to the user.
  --
  -- In practice each issue requires a maximum of two different representations:
  -- based on the context the issue might need to be rendered as "opposite" ('Backward')
  -- – for example when rendering non-breaking changes everything should be
  -- reversed (a consequence of the way we generate non-breaking changes).
  --
  -- If _consumer_ doesn't have something, the element was "removed".
  -- If _producer_ doesn't have something, the element was "added".
  describeIssue :: Orientation -> Issue l -> Blocks

  issueKind :: Issue l -> IssueKind

data Orientation = Forward | Backward
  deriving stock (Eq, Ord, Show)

toggleOrientation :: Orientation -> Orientation
toggleOrientation Forward = Backward
toggleOrientation Backward = Forward

-- | A set of interactions having common unifying features
type Behavior = Paths Behave 'APILevel

instance Issuable l => ToJSON (Issue l) where
  toJSON = toJSON . show

data AnIssue (l :: BehaviorLevel) where
  AnIssue :: Issuable l => Orientation -> Issue l -> AnIssue l

deriving stock instance Show (AnIssue l)

deriving stock instance Eq (AnIssue l)

deriving stock instance Ord (AnIssue l)

instance ToJSON (AnIssue l) where
  toJSON (AnIssue _ issue) = toJSON issue
