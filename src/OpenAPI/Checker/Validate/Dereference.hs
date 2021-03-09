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
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi.Internal
import qualified Data.Text as T
import Data.Unique
import Generic.Data

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

-- | Modifies all references in a structure and keys in components to be unique
-- across the run of the binary.
--
-- This is useful for merging multiple schemas together (or only parts of them)
-- While still resolving references to the correct components.
--
-- Unfortunately, 'Components' does not have 'Reference' as keys, which seems
-- like it would be the right thing to do.
uniqRefs :: HasTypes s Reference => Components -> s -> IO (UniqComponents, s)
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
      s' = s & types @Reference . coerced %~ modification
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
