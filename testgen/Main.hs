module Main
  ( main
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import Data.Yaml hiding (Parser)
import Options.Applicative
import Options.Applicative.Help (vcat, paragraph, extractChunk)
import System.Directory
import System.FilePath
import System.IO
import System.Random
import System.Random.Stateful

import Components

data Options = Options
  { optBaseDir :: FilePath
  , optMainTypeVariant :: (ComponentType, ComponentVariant)
  , optOverrides :: [(ComponentType, ComponentVariant)]
  , optOutputDir :: FilePath
  , optSeed :: Maybe Int
  , optCount :: Int
  }

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parser) (briefDesc <> header "Random YAML tree generator" <> progDescDoc (Just $
    vcat
      [ extractChunk . paragraph $ "Generates pairs of YAML trees in <outputDir> adhering to a given specification."
      , extractChunk . paragraph $ "The tree starts at the root.yaml file in the base directory, and whenever we\
        \ encounter a string \"$<name>\", we insert one of the YAML files among <name>/*/a.yaml (at random)."
      , extractChunk . paragraph $ "The first tree has all references to <mainName> resolved to\
        \ <mainName>/<mainVariant>/a.yaml, and the second tree will have those resolved to \
        \ <mainName>/<mainVariant>/b.yaml"
      , extractChunk . paragraph $ "An string \"$<name>=<id>\" is the same as \"$<name>\", except all references with\
        \ the same id are resolved to the same file."
      , extractChunk . paragraph $ "Every pair of trees is saved under <outputDir>/<sha1>.<seed>/{a,b}.yaml"
      ]
    ))

  seed <- case optSeed opts of
    Just seed -> pure seed
    Nothing -> randomIO
  hPutStrLn stderr $ "Using seed: " <> show seed
  gen <- newIOGenM $ mkStdGen seed

  replicateM_ (optCount opts) $ do
    seed' <- randomM gen -- not using splitGenM because we want a "serializable" seed
    (treeA, treeB) <- readTreePair (mkStdGen seed') (optBaseDir opts) (optMainTypeVariant opts) (M.fromList $ optOverrides opts)
    let bsA = encode treeA
    let bsB = encode treeB
    let hash = B16.encode $ SHA1.hash (bsA <> bsB)
    let basename = optOutputDir opts </> (BSC.unpack hash <> "." <> show seed')
    createDirectoryIfMissing True basename
    BS.writeFile (basename </> "a.yaml") bsA
    BS.writeFile (basename </> "b.yaml") bsB
  where
    parser = Options <$> baseDirOpt <*> tyVarOpt <*> overrideOpts <*> outputOpt <*> seedOpt <*> countOpt
    baseDirOpt :: Parser FilePath
    baseDirOpt = strOption (long "base-dir" <> metavar "<baseDir>" <> value "." <> showDefaultWith id
      <> help "The base directory for YAML files")
    readOverride = eitherReader $ \xs -> case break (== '=') xs of
      (name, '=':var) -> Right (name, var)
      _ -> Left "Expected name=variant"
    tyVarOpt :: Parser (ComponentType, ComponentVariant)
    tyVarOpt = argument readOverride (metavar "<mainName>=<mainVariant>")
    overrideOpts :: Parser [(ComponentType, ComponentVariant)]
    overrideOpts = many $ option readOverride (long "override" <> metavar "<name>=<variant>"
      <> help "Ensure that references to <name> are resolved to <variant> (multiple allowed)")
    outputOpt :: Parser FilePath
    outputOpt = strOption (long "output" <> metavar "<outputDir>" <> value "." <> showDefaultWith id
      <> help "Output directory")
    seedOpt :: Parser (Maybe Int)
    seedOpt = optional $ option auto (long "seed" <> metavar "<seed>" <> help "The seed used to generate seeds")
    countOpt :: Parser Int
    countOpt = option auto (short 'c' <> metavar "<count>" <> value 10 <> showDefault
      <> help "How many pairs of trees to generate")
