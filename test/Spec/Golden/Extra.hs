module Spec.Golden.Extra
  ( getGoldenInputs
  , getGoldenInputsUniform
  , goldenInputsTree
  , goldenInputsTreeUniform
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml as Yaml
import OpenAPI.Checker.Subtree
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Providers

data TestInput t
  = TestInputNode TestName [TestInput t]
  | TestInputLeaf TestName t FilePath
  deriving (Functor)

getGoldenInputs
  :: (Each s t (FilePath, FilePath -> IO a) a)
  => TestName
  -> FilePath
  -> s
  -> IO (TestInput t)
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

getGoldenInputsUniform
  :: (Each t h (FilePath, FilePath -> IO a) a)
  => (Each s t FilePath (FilePath, FilePath -> IO a))
  => TestName
  -> (FilePath -> IO a)
  -> FilePath
  -> s
  -> IO (TestInput h)
getGoldenInputsUniform name f filepath inp = getGoldenInputs name filepath $ inp & each %~ (,f)

goldenInputsTree
  :: (Each s t (FilePath, FilePath -> IO a) a, ToJSON x, HasUnsupportedFeature x)
  => TestName
  -> FilePath -- ^ Root path
  -> FilePath -- ^ Name of golden file
  -> s
  -> (t -> x)
  -> IO TestTree
goldenInputsTree name filepath golden inp f = do
  runTestInputTree golden f <$> getGoldenInputs name filepath inp

runTestInputTree
  :: (ToJSON x, HasUnsupportedFeature x)
  => FilePath
  -> (t -> x)
  -> TestInput t
  -> TestTree
runTestInputTree golden f (TestInputNode name rest) =
  testGroup name (runTestInputTree golden f <$> rest)
runTestInputTree golden f (TestInputLeaf name t path)
  | testSupported name =
    goldenVsStringDiff
      name
      (\ref new -> ["diff", "-u", ref, new])
      (path </> golden)
      (pure . BSL.fromStrict . Yaml.encode . f $ t)
runTestInputTree _ f (TestInputLeaf name t _) =
  reportResult name $
    if hasUnsupportedFeature (f t)
      then testPassed "Feature unsupported"
      else testFailed "Unexpected feature support"

reportResult :: TestName -> Result -> TestTree
reportResult name result = singleTest name $ SimpleTestReporter result

newtype SimpleTestReporter = SimpleTestReporter Result

instance IsTest SimpleTestReporter where
  run _ (SimpleTestReporter result) _ = return result
  testOptions = mempty

testSupported :: TestName -> Bool
testSupported ('x' : ' ' : _) = False
testSupported _ = True

goldenInputsTreeUniform
  :: ( Each t h (FilePath, FilePath -> IO a) a
     , ToJSON x
     , Each s t FilePath (FilePath, FilePath -> IO a)
     , HasUnsupportedFeature x
     )
  => String
  -> FilePath -- ^ Root path
  -> FilePath -- ^ Name of golden file
  -> s
  -> (FilePath -> IO a)
  -> (h -> x)
  -> IO TestTree
goldenInputsTreeUniform name filepath golden inp h =
  goldenInputsTree name filepath golden (inp & each %~ (,h))
