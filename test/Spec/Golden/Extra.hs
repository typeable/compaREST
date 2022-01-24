module Spec.Golden.Extra
  ( getGoldenInputs,
    getGoldenInputsUniform,
    goldenInputsTree,
    goldenInputsTreeUniform,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

data TestInput t
  = TestInputNode TestName [TestInput t]
  | TestInputLeaf TestName t FilePath
  deriving stock (Functor)

getGoldenInputs ::
  (Each s t (FilePath, FilePath -> IO a) a) =>
  TestName ->
  FilePath ->
  s ->
  IO (TestInput t)
getGoldenInputs name filepath inp = do
  dirs' <- listDirectory filepath >>= filterM (doesDirectoryExist . (filepath </>))
  case dirs' of
    -- A test
    [] -> do
      x <-
        inp & each %%~ \(file, f) ->
          f $ filepath </> file
      return $ TestInputLeaf name x filepath
    -- A test group
    dirs ->
      TestInputNode name
        <$> forM dirs (\dir -> getGoldenInputs dir (filepath </> dir) inp)

getGoldenInputsUniform ::
  (Each t h (FilePath, FilePath -> IO a) a) =>
  (Each s t FilePath (FilePath, FilePath -> IO a)) =>
  TestName ->
  (FilePath -> IO a) ->
  FilePath ->
  s ->
  IO (TestInput h)
getGoldenInputsUniform name f filepath inp = getGoldenInputs name filepath $ inp & each %~ (,f)

goldenInputsTree ::
  (Each s t (FilePath, FilePath -> IO a) a) =>
  TestName ->
  -- | Root path
  FilePath ->
  -- | Name of golden file
  FilePath ->
  s ->
  (t -> IO BSL.ByteString) ->
  IO TestTree
goldenInputsTree name filepath golden inp f = do
  runTestInputTree golden f <$> getGoldenInputs name filepath inp

runTestInputTree ::
  FilePath ->
  (t -> IO BSL.ByteString) ->
  TestInput t ->
  TestTree
runTestInputTree golden f (TestInputNode name rest) =
  testGroup name (runTestInputTree golden f <$> rest)
runTestInputTree golden f (TestInputLeaf name t path) =
  goldenVsStringDiff
    name
    (\ref new -> ["diff", "-u", ref, new])
    (path </> golden)
    (f t)

goldenInputsTreeUniform ::
  ( Each t h (FilePath, FilePath -> IO a) a
  , Each s t FilePath (FilePath, FilePath -> IO a)
  ) =>
  String ->
  -- | Root path
  FilePath ->
  -- | Name of golden file
  FilePath ->
  s ->
  (FilePath -> IO a) ->
  (h -> IO BSL.ByteString) ->
  IO TestTree
goldenInputsTreeUniform name filepath golden inp h =
  goldenInputsTree name filepath golden (inp & each %~ (,h))
