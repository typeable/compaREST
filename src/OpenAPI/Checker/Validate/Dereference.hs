module OpenAPI.Checker.Validate.Dereference
  ( dereferenceParam,
    ResolvableComponent (..),
    UniqComponents (..),
    UniqDefinitions,
    getUniqDefinitions,
    uniqRefs,
  )
where

import Control.Applicative
import Control.Lens
import Data.Generics.Product.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHS
import Data.OpenApi.Internal
import Data.Scientific
import qualified Data.Text as T
import Data.Unique
import Data.Vector (Vector)
import Generic.Data
import Network.HTTP.Media
import OpenAPI.Orphans ()

-- | 'Definitions', but already has the unqieness tag applied to it.
newtype UniqDefinitions a = UniqDefinitions {unUniqDefinitions :: Definitions a}
  deriving newtype (Eq, Show, Semigroup, Monoid)

getUniqDefinitions :: UniqDefinitions a -> Definitions a
getUniqDefinitions = unUniqDefinitions

data UniqComponents = UniqComponents
  { uniqComponentsSchemas :: UniqDefinitions Schema,
    uniqComponentsResponses :: UniqDefinitions Response,
    uniqComponentsParameters :: UniqDefinitions Param,
    uniqComponentsExamples :: UniqDefinitions Example,
    uniqComponentsRequestBodies :: UniqDefinitions RequestBody,
    uniqComponentsHeaders :: UniqDefinitions Header,
    uniqComponentsSecuritySchemes :: UniqDefinitions SecurityScheme,
    uniqComponentsLinks :: UniqDefinitions Link,
    uniqComponentsCallbacks :: UniqDefinitions Callback
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically UniqComponents)

data ReferencesTraversal

type family ReferencesTraversalChildren a where
  ReferencesTraversalChildren URL = '[]
  ReferencesTraversalChildren MediaType = '[]
  ReferencesTraversalChildren Scientific = '[]
  ReferencesTraversalChildren (Vector v) = '[v]
  ReferencesTraversalChildren (HM.HashMap k v) = '[k, v]
  ReferencesTraversalChildren (IOHM.InsOrdHashMap k v) = '[k, v]
  ReferencesTraversalChildren (IOHS.InsOrdHashSet v) = '[v]
  ReferencesTraversalChildren a = Children ChGeneric a

instance
  HasTypesCustom
    ReferencesTraversal
    v
    v'
    a
    b =>
  HasTypesCustom
    ReferencesTraversal
    (IOHM.InsOrdHashMap k v)
    (IOHM.InsOrdHashMap k v')
    a
    b
  where
  typesCustom = traverse . typesCustom @ReferencesTraversal

type instance Children ReferencesTraversal a = ReferencesTraversalChildren a

-- | Modifies all references in a structure and keys in components to be unique
-- across the run of the binary.
--
-- This is useful for merging multiple schemas together (or only parts of them)
-- While still resolving references to the correct components.
--
-- Unfortunately, 'Components' does not have 'Reference' as keys, which seems
-- like it would be the right thing to do.
uniqRefs ::
  HasTypesUsing ReferencesTraversal s s Reference Reference =>
  Components ->
  s ->
  IO (UniqComponents, s)
uniqRefs cs s = do
  u <- T.pack . (<> " ") . show . hashUnique <$> newUnique
  let -- References seem to be unescaped url fragments, so a space is a unique delimiter.
      modification = (u <>)
      getComponent :: (Components -> Definitions x) -> UniqDefinitions x
      getComponent f = UniqDefinitions . IOHM.mapKeys modification . f $ cs
      ucs =
        UniqComponents
          (getComponent _componentsSchemas)
          (getComponent _componentsResponses)
          (getComponent _componentsParameters)
          (getComponent _componentsExamples)
          (getComponent _componentsRequestBodies)
          (getComponent _componentsHeaders)
          (getComponent _componentsSecuritySchemes)
          (getComponent _componentsLinks)
          (getComponent _componentsCallbacks)
      s' = s & typesUsing @ReferencesTraversal @Reference . coerced %~ modification
  return (ucs, s')

class ResolvableComponent x where
  getMapping :: UniqComponents -> UniqDefinitions x

instance ResolvableComponent Schema where
  getMapping = uniqComponentsSchemas

instance ResolvableComponent Response where
  getMapping = uniqComponentsResponses

instance ResolvableComponent Param where
  getMapping = uniqComponentsParameters

instance ResolvableComponent Example where
  getMapping = uniqComponentsExamples

instance ResolvableComponent RequestBody where
  getMapping = uniqComponentsRequestBodies

instance ResolvableComponent Header where
  getMapping = uniqComponentsHeaders

instance ResolvableComponent SecurityScheme where
  getMapping = uniqComponentsSecuritySchemes

instance ResolvableComponent Link where
  getMapping = uniqComponentsLinks

instance ResolvableComponent Callback where
  getMapping = uniqComponentsCallbacks

-- | Throws error if param not found
dereferenceParam ::
  (Alternative m, ResolvableComponent x) =>
  UniqComponents ->
  Referenced x ->
  m x
dereferenceParam _ (Inline x) = pure x
dereferenceParam cs (Ref (Reference k)) =
  maybe empty pure . IOHM.lookup k . unUniqDefinitions . getMapping $ cs
