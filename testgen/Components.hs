module Components
  ( ComponentType
  , ComponentVariant
  , readTreePair
  ) where

import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson hiding ((.=))
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import System.Random

import Yaml

data AorB = A | B
  deriving stock (Eq, Ord, Show)

type ComponentType = String
type ComponentVariant = String

data Component = Component
  { componentType :: ComponentType
  , componentVariant :: ComponentVariant
  , componentAB :: AorB
  }
  deriving stock (Eq, Ord, Show)

componentFilePath :: FilePath -> Component -> FilePath
componentFilePath baseDir Component{..} = baseDir </> componentType </> componentVariant </> case componentAB of
  A -> "a.yaml"
  B -> "b.yaml"

componentDirectory :: ComponentType -> FilePath
componentDirectory k = k

randomVariant :: FilePath -> ComponentType -> IO ComponentVariant
randomVariant baseDir k = listDirectory (baseDir </> componentDirectory k) >>= \case
  [] -> error $ "No variants for " <> k
  xs -> (xs !!) <$> randomRIO (0, length xs - 1)

data AnchorOccurrence = All | Named String | Unnamed Int
  deriving stock (Eq, Ord, Show)

type AnchorOccurrences = M.Map ComponentType Int

type ComponentChoices = M.Map (ComponentType, AnchorOccurrence) (ComponentVariant, AorB)

type ParseM = ReaderT FilePath (StateT (AnchorOccurrences, ComponentChoices) IO)

nextIndex :: ComponentType -> ParseM Int
nextIndex k = _1 . iat k %%= ((,) <*> Just) . maybe 0 (+ 1)

parseAnchor :: String -> ParseM (ComponentType, AnchorOccurrence)
parseAnchor xs = case break (== '-') xs of
  (k, '-':name) -> pure $ (k, Named name)
  (k, _) -> (k,) . Unnamed <$> nextIndex k

chooseComponent :: (ComponentType, AnchorOccurrence) -> ParseM Component
chooseComponent (k, occ) = do
  (var, aorb) <- use (_2 . iat (k, All)) >>= \case
    Just varOcc -> pure varOcc
    Nothing -> use (_2 . iat (k, occ)) >>= \case
      Just varOcc -> pure varOcc
      Nothing -> do
        baseDir <- ask
        var <- liftIO $ randomVariant baseDir k
        _2 . iat (k, occ) .= Just (var, A)
        pure (var, A)
  pure Component
    { componentType = k
    , componentVariant = var
    , componentAB = aorb
    }

parseTree :: ParseM Value
parseTree = do
  baseDir <- ask
  readYamlTree resolveAnchor $ baseDir </> "root.yaml"
  where
    resolveAnchor k = do
      tyOcc <- parseAnchor k
      component <- chooseComponent tyOcc
      baseDir <- ask
      pure $ componentFilePath baseDir component

readTreePair :: FilePath -> ComponentType -> ComponentVariant -> M.Map ComponentType ComponentVariant -> IO (Value, Value)
readTreePair baseDir mainK mainVar overrides = do
  (treeA, (_, choices)) <- runStateT (runReaderT parseTree baseDir) (M.empty, M.insert (mainK, All) (mainVar, A) initChoices)
  (treeB, _) <- runStateT (runReaderT parseTree baseDir) (M.empty, M.insert (mainK, All) (mainVar, B) choices)
  pure (treeA, treeB)
  where
    initChoices = M.mapKeysMonotonic (,All) $ M.map (,A) overrides
