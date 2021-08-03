module Main
  ( main
  ) where

import Options.Applicative
import Options.Applicative.Help (vcat, paragraph, extractChunk)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Yaml hiding (Parser)
import System.IO
import System.Random

import Components

data Options = Options
  { optBaseDir :: FilePath
  , optMainTypeVariant :: (ComponentType, ComponentVariant)
  , optOverrides :: [(ComponentType, ComponentVariant)]
  }

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parser) (briefDesc <> header "Random YAML tree generator" <> progDescDoc (Just $
    vcat
      [ extractChunk . paragraph $ "Generates a pair of YAML trees, one to stdout, one to stderr, adhering to a given\
        \ specification."
      , extractChunk . paragraph $ "The tree starts at the root.yaml file in the base directory, and whenever we\
        \ encounter a YAML alias \"*<name>\", we insert one of the YAML files among <name>/*/a.yaml (at random)."
      , extractChunk . paragraph $ "The first tree has all references to <mainName> resolved to\
        \ <mainName>/<mainVariant>/a.yaml, and the second tree will have those resolved to \
        \ <mainName>/<mainVariant>/b.yaml"
      , extractChunk . paragraph $ "An alias \"*<name-id>\" is the same as \"*<name>\", except all references with the\
        \ same id are resolved to the same file."
      ]
    ))
  gen <- newStdGen
  (treeA, treeB) <- readTreePair gen (optBaseDir opts) (optMainTypeVariant opts) (M.fromList $ optOverrides opts)
  BS.hPut stdout $ encode treeA
  BS.hPut stderr $ encode treeB
  where
    parser = Options <$> baseDirOpt <*> tyVarOpt <*> overrideOpts
    baseDirOpt :: Parser FilePath
    baseDirOpt = strOption (long "base-dir" <> metavar "<baseDir>" <> value "." <> showDefaultWith id
      <> help "The base directory for YAML files")
    tyVarOpt :: Parser (ComponentType, ComponentVariant)
    tyVarOpt = argument readOverride (metavar "<mainName>=<mainVariant>")
    overrideOpts :: Parser [(ComponentType, ComponentVariant)]
    overrideOpts = many $ option readOverride (long "override" <> metavar "<name>=<variant>"
      <> help "Ensure that references to <name> are resolved to <variant> (multiple allowed)")
    readOverride = eitherReader $ \xs -> case break (== '=') xs of
      (name, '=':var) -> Right (name, var)
      _ -> Left "Expected name=variant"
