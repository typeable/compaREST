module Spec.Golden.Extra
  ( getGoldenInputs,
    getGoldenInputsUniform,
    goldenInputsTree,
    goldenInputsTreeUniform,
  )
where

import Control.Lens
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

data GoldenMetadata = GoldenMetadata
  { directoryName :: FilePath,
    directoryPath :: FilePath
  }

getGoldenInputs ::
  (Each s t (FilePath, FilePath -> IO a) a) =>
  FilePath ->
  s ->
  IO [(t, GoldenMetadata)]
getGoldenInputs filepath inp = do
  dirs <- listDirectory filepath >>= filterM (doesDirectoryExist . (filepath </>))
  forM dirs $ \directoryName -> do
    let directoryPath = filepath </> directoryName
    x <-
      inp & each %%~ \(file, f) -> do
        f $ directoryPath </> file
    return
      (x, GoldenMetadata {..})

getGoldenInputsUniform ::
  (Each t h (FilePath, FilePath -> IO a) a) =>
  (Each s t FilePath (FilePath, FilePath -> IO a)) =>
  (FilePath -> IO a) ->
  FilePath ->
  s ->
  IO [(h, GoldenMetadata)]
getGoldenInputsUniform f filepath inp = getGoldenInputs filepath $ inp & each %~ (,f)

goldenInputsTree ::
  (Each s t (FilePath, FilePath -> IO a) a) =>
  String ->
  -- | Root path
  FilePath ->
  -- | Name of golden file
  FilePath ->
  s ->
  (t -> ByteString) ->
  IO TestTree
goldenInputsTree name filepath golden inp f = do
  x <- getGoldenInputs filepath inp
  return $
    testGroup name $
      x <&> \(t, GoldenMetadata {..}) ->
        goldenVsStringDiff
          directoryName
          (\ref new -> ["diff", "-u", ref, new])
          (directoryPath </> golden)
          (pure . f $ t)

goldenInputsTreeUniform ::
  (Each t h (FilePath, FilePath -> IO a) a) =>
  (Each s t FilePath (FilePath, FilePath -> IO a)) =>
  String ->
  -- | Root path
  FilePath ->
  -- | Name of golden file
  FilePath ->
  s ->
  (FilePath -> IO a) ->
  (h -> ByteString) ->
  IO TestTree
goldenInputsTreeUniform name filepath golden inp h =
  goldenInputsTree name filepath golden (inp & each %~ (,h))
