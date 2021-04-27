{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.ProcessedPathItem
  ( ProcessedPathItem (..)
  , ProcessedPathItems (..)
  , processPathItems
  , Step (..)
  )
where

import Control.Monad
import Data.Foldable as F
import Data.Functor
import Data.HList
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.Text (Text)
import Data.Text as T
import Generic.Data
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Operation
import OpenAPI.Checker.Validate.PathFragment
import OpenAPI.Checker.Validate.Sums

-- FIXME: There's probably a better name for this, but `PathItem` is already taken ;(
data ProcessedPathItem = ProcessedPathItem
  { path :: FilePath
  , item :: PathItem
  }

processPathItems :: [(FilePath, PathItem)] -> ProcessedPathItems
processPathItems = ProcessedPathItems . fmap (uncurry ProcessedPathItem)

newtype ProcessedPathItems =
  ProcessedPathItems {unProcessedPathItems :: [ProcessedPathItem]}

instance Subtree ProcessedPathItems where
  type
    CheckEnv ProcessedPathItems =
      '[ ProdCons (Definitions Param)
       , ProdCons (Definitions RequestBody)
       , ProdCons (Definitions SecurityScheme)
       , ProdCons (Definitions Response)
       , ProdCons (Definitions Header)
       , ProdCons (Definitions Schema)
       ]
  data CheckIssue ProcessedPathItems
    = NoPathsMatched FilePath
    | AllPathsFailed FilePath
    -- When several paths match given but all checks failed
    deriving (Eq, Ord, Show)
  checkCompatibility env (ProdCons p c) = do
    -- Each path generated by producer must be handled by consumer with exactly
    -- one way
    for_ (unProcessedPathItems p) $ \ prodItem -> do
      let
        prodPath = path prodItem
        matchedItems = do
          consItem <- unProcessedPathItems c
          matched <- F.toList $ matchingPathItems $ ProdCons prodItem consItem
          return matched
      case matchedItems of
        [] -> issueAt producer $ NoPathsMatched prodPath
        [matched] -> do
          -- Checking exact match with no wrapper
          let trace = matchedTrace <$> matched
          localTrace trace $ checkCompatibility env matched
        matches -> anyOfAt consumer (AllPathsFailed prodPath) $ do
          match <- matches
          let trace = matchedTrace <$> match
          pure $ localTrace trace $ checkCompatibility env match
    where
      matchedTrace :: MatchedPathItem -> Trace ProcessedPathItems MatchedPathItem
      matchedTrace mpi = step $ MatchedPathStep $ matchedPath mpi

-- | Preliminary checks two paths for compatibility.  Returns Nothing if two
-- paths obviously do not match: static parts differ or count of path elements
-- is not equal
matchingPathItems :: ProdCons ProcessedPathItem -> Maybe (ProdCons MatchedPathItem)
matchingPathItems prodCons = do
  let frags = parsePath . path <$> prodCons
  guard $ fragsMatch frags
  let
    mkMatchedItems frag ppi = MatchedPathItem
      { pathItem = item ppi
      , matchedPath = path ppi
      , pathFragments = frag }
  return $ mkMatchedItems <$> frags <*> prodCons

fragsMatch :: ProdCons [PathFragment Text] -> Bool
fragsMatch (ProdCons p c) = maybe False and $ zipAllWith check p c
  where
    check (StaticPath s1) (StaticPath s2) = s1 == s2
    check _ _ = True

zipAllWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipAllWith _ [] [] = Just []
zipAllWith f (x : xs) (y : ys) = (f x y :) <$> zipAllWith f xs ys
zipAllWith _ (_ : _) [] = Nothing
zipAllWith _ [] (_ : _) = Nothing

data MatchedPathItem = MatchedPathItem
  { pathItem :: !PathItem
  , matchedPath :: !FilePath
  , pathFragments :: ![PathFragment Text]
  -- ^ Pre-parsed path from PathItem
  }

instance Subtree MatchedPathItem where
  type CheckEnv MatchedPathItem =
    '[ ProdCons (Definitions Param)
     , ProdCons (Definitions RequestBody)
     , ProdCons (Definitions SecurityScheme)
     , ProdCons (Definitions Response)
     , ProdCons (Definitions Header)
     , ProdCons (Definitions Schema)
     ]
  data CheckIssue MatchedPathItem
    = OperationMissing (Step MatchedPathItem MatchedOperation)
    deriving (Eq, Ord, Show)
  checkCompatibility env prodCons = withTrace $ \rootTrace -> do
    let
      paramDefs = getH @(ProdCons (Definitions Param)) env
      pathTracedParams = getPathParams <$> rootTrace <*> paramDefs <*> prodCons
      getPathParams
        :: Trace OpenApi MatchedPathItem
        -> Definitions Param
        -> MatchedPathItem
        -> [Traced OpenApi Param]
      getPathParams root defs mpi = do
        paramRef <- _pathItemParameters $ pathItem mpi
        let
          traced = dereferenceTraced defs
            $ Traced (step PathItemParam) paramRef
          res = retrace root traced
        pure res
      pathTracedFragments = mkPathFragments <$> rootTrace <*> prodCons
      mkPathFragments myRoot mpi operationParams =
        --  operationParams will be known on Operation check stage, so we give a
        --  function, returning fragments
        let
          paramsMap :: Map Text (Traced OpenApi Param)
          paramsMap = M.fromList $ do
            tracedParam <- operationParams
            let pname = _paramName $ getTraced tracedParam
            pure (pname, tracedParam)
          fragments :: [PathFragmentParam]
          fragments = (pathFragments mpi) <&> \case
            StaticPath t -> StaticPath t
            DynamicPath pname -> DynamicPath
              $ fromMaybe (error $ "Param not found " <> T.unpack pname)
              $ M.lookup pname paramsMap
          tracedFragments :: [Traced OpenApi PathFragmentParam]
          tracedFragments = L.zip [0..] fragments <&> \(pos, frag) ->
            retrace myRoot $ Traced (step $ PathFragmentStep pos) frag
        in tracedFragments
      operations = getOperations <$> pathTracedParams <*> pathTracedFragments <*> prodCons
      getOperations pathParams getPathFragments mpi = M.fromList $ do
        (get, s) <-
          [ (_pathItemGet, GetStep)
          , (_pathItemPut, PutStep)
          , (_pathItemPost, PostStep)
          , (_pathItemDelete, DeleteStep)
          , (_pathItemOptions, OptionsStep)
          , (_pathItemHead, HeadStep)
          , (_pathItemPatch, PatchStep)
          , (_pathItemTrace, TraceStep) ]
        operation <- F.toList $ get $ pathItem mpi
        -- Got only Justs here
        let mop = MatchedOperation { operation , pathParams, getPathFragments }
        pure (s, Traced (step s) mop)
      check _ pc = checkCompatibility @MatchedOperation env pc
    -- Operations are sum-like entities. Use step to operation as key because
    -- why not
    checkSums OperationMissing check operations


instance Steppable ProcessedPathItems MatchedPathItem where
  data Step ProcessedPathItems MatchedPathItem = MatchedPathStep FilePath
    deriving (Eq, Ord, Show)

instance Steppable MatchedPathItem MatchedOperation where
  data Step MatchedPathItem MatchedOperation
    = GetStep
    | PutStep
    | PostStep
    | DeleteStep
    | OptionsStep
    | HeadStep
    | PatchStep
    | TraceStep
    deriving (Eq, Ord, Show)

instance Steppable MatchedPathItem (Referenced Param) where
  data Step MatchedPathItem (Referenced Param) = PathItemParam
    deriving (Eq, Ord, Show)

instance Steppable MatchedPathItem PathFragmentParam where
  data Step MatchedPathItem PathFragmentParam = PathFragmentStep Int
    deriving (Eq, Ord, Show)
