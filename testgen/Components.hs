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
import Data.List (sort)
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import System.Random.Stateful

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

randomVariant :: IOGenM StdGen -> FilePath -> ComponentType -> IO ComponentVariant
randomVariant gen baseDir k = listDirectory (baseDir </> componentDirectory k) >>= \case
  [] -> error $ "No variants for " <> k
  xs -> (sort xs !!) <$> randomRM (0, length xs - 1) gen

type ComponentChoices = M.Map (ComponentType, String) ComponentVariant
type ComponentOverrides = M.Map ComponentType (ComponentVariant, AorB)

type ParseM = ReaderT (FilePath, ComponentOverrides, IOGenM StdGen) (StateT ComponentChoices IO)

chooseComponent :: Reference -> ParseM Component
chooseComponent (Labelled key ident) = do
  (var, aorb) <- view (_2 . iat key) >>= \case
    Just (var, aorb) -> pure (var, aorb)
    Nothing -> use (iat (key, ident)) >>= \case
      Just var -> pure (var, A)
      Nothing -> do
        baseDir <- view _1
        gen <- view _3
        var <- liftIO $ randomVariant gen baseDir key
        iat (key, ident) .= Just var
        pure (var, A)
  pure Component
    { componentType = key
    , componentVariant = var
    , componentAB = aorb
    }
chooseComponent (Unlabelled key) = do
  (var, aorb) <- view (_2 . iat key) >>= \case
    Just (var, aorb) -> pure (var, aorb)
    Nothing -> do
      baseDir <- view _1
      gen <- view _3
      var <- liftIO $ randomVariant gen baseDir key
      pure (var, A)
  pure Component
    { componentType = key
    , componentVariant = var
    , componentAB = aorb
    }

parseTree :: ParseM Value
parseTree = do
  baseDir <- view _1
  readYamlTree resolveRef $ baseDir </> "root.yaml"
  where
    resolveRef ref = do
      component <- chooseComponent ref
      baseDir <- view _1
      pure $ componentFilePath baseDir component

readTreePair
  :: StdGen
  -> FilePath
  -> (ComponentType, ComponentVariant)
  -> M.Map ComponentType ComponentVariant
  -> IO (Value, Value)
readTreePair gen baseDir (mainK, mainVar) overrides = do
  genA <- newIOGenM gen
  treeA <- evalStateT (runReaderT parseTree (baseDir, M.insert mainK (mainVar, A) overrides', genA)) M.empty
  genB <- newIOGenM gen
  treeB <- evalStateT (runReaderT parseTree (baseDir, M.insert mainK (mainVar, B) overrides', genB)) M.empty
  pure (treeA, treeB)
  where
    overrides' = M.map (,A) overrides
