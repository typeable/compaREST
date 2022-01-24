{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Data.OpenApi.Compare.Validate.Operation
  ( -- * Operation
    MatchedOperation (..),
    OperationMethod (..),
    pathItemMethod,

    -- * ProcessedPathItem
    ProcessedPathItem (..),
    ProcessedPathItems (..),
    processPathItems,
    Step (..),
    Behave (..),
    Issue (..),
  )
where

import Control.Arrow
import Control.Comonad.Env
import Control.Monad
import Data.Foldable as F
import Data.Functor
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Compare.Behavior
import Data.OpenApi.Compare.References
import Data.OpenApi.Compare.Subtree
import Data.OpenApi.Compare.Validate.MediaTypeObject
import Data.OpenApi.Compare.Validate.OAuth2Flows
import Data.OpenApi.Compare.Validate.PathFragment
import Data.OpenApi.Compare.Validate.Products
import Data.OpenApi.Compare.Validate.RequestBody
import Data.OpenApi.Compare.Validate.Responses
import Data.OpenApi.Compare.Validate.SecurityRequirement ()
import Data.OpenApi.Compare.Validate.Server ()
import Data.OpenApi.Compare.Validate.Sums
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder

data MatchedOperation = MatchedOperation
  { operation :: !Operation
  , -- | Params from the PathItem
    pathParams :: ![Traced Param]
  , -- | Path fragments traced from PathItem. Takes full list of
    -- operation-specific parameters
    getPathFragments :: !([Traced Param] -> [Traced PathFragmentParam])
  }

type ParamKey = (ParamLocation, Text)

paramKey :: Param -> ParamKey
paramKey param = (_paramIn param, _paramName param)

tracedParameters :: Traced MatchedOperation -> [Traced (Referenced Param)]
tracedParameters oper =
  [ traced (ask oper >>> step (OperationParamsStep i)) x
  | (i, x) <- zip [0 ..] $ _operationParameters . operation $ extract oper
  ]

tracedRequestBody :: Traced MatchedOperation -> Maybe (Traced (Referenced RequestBody))
tracedRequestBody oper = _operationRequestBody (operation $ extract oper) <&> traced (ask oper >>> step OperationRequestBodyStep)

tracedResponses :: Traced MatchedOperation -> Traced Responses
tracedResponses oper =
  traced (ask oper >>> step OperationResponsesStep) $
    _operationResponses . operation $ extract oper

tracedSecurity :: Traced MatchedOperation -> [(Int, Traced SecurityRequirement)]
tracedSecurity oper =
  [ (i, traced (ask oper >>> step (OperationSecurityRequirementStep i)) x)
  | (i, x) <- zip [0 ..] $ _operationSecurity . operation $ extract oper
  ]

tracedCallbacks :: Traced MatchedOperation -> [(Text, Traced (Referenced Callback))]
tracedCallbacks (Traced t oper) =
  [ (k, Traced (t >>> step (OperationCallbackStep k)) v)
  | (k, v) <- IOHM.toList . _operationCallbacks . operation $ oper
  ]

-- FIXME: #28
getServers ::
  -- | Servers from env
  [Server] ->
  MatchedOperation ->
  [Server]
getServers env oper =
  case _operationServers . operation $ oper of
    [] -> env
    ss -> ss

instance Behavable 'OperationLevel 'PathFragmentLevel where
  data Behave 'OperationLevel 'PathFragmentLevel
    = InParam Text
    | InFragment (PathFragment Text)
    deriving stock (Eq, Ord, Show)
  describeBehavior (InParam p) = "Parameter " <> text p
  describeBehavior (InFragment (StaticPath p)) = "Static fragment " <> code p
  describeBehavior (InFragment (DynamicPath p)) = "Dynamic fragment " <> code p

instance Behavable 'OperationLevel 'RequestLevel where
  data Behave 'OperationLevel 'RequestLevel
    = InRequest
    deriving stock (Eq, Ord, Show)
  describeBehavior InRequest = "Request"

instance Behavable 'OperationLevel 'SecurityRequirementLevel where
  data Behave 'OperationLevel 'SecurityRequirementLevel
    = SecurityRequirementStep Int
    deriving stock (Eq, Ord, Show)
  describeBehavior (SecurityRequirementStep i) =
    "Security requirement " <> (text . T.pack . show $ i)

instance Subtree MatchedOperation where
  type SubtreeLevel MatchedOperation = 'OperationLevel
  type
    CheckEnv MatchedOperation =
      '[ ProdCons (Traced (Definitions Param))
       , ProdCons (Traced (Definitions RequestBody))
       , ProdCons (Traced (Definitions SecurityScheme))
       , ProdCons (Traced (Definitions Response))
       , ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons [Server]
       , ProdCons (Traced (Definitions Link))
       , ProdCons (Traced (Definitions Callback))
       ]
  checkStructuralCompatibility env pc = do
    let pParams :: ProdCons [Traced Param]
        pParams = do
          defs <- getH @(ProdCons (Traced (Definitions Param))) env
          op' <- tracedParameters <$> pc
          pp <- pathParams . extract <$> pc
          pure $
            let o = M.fromList $ do
                  param <- dereference defs <$> op'
                  let key = paramKey . extract $ param
                  pure (key, param)
                p = M.fromList $ do
                  param <- pp
                  pure (paramKey . extract $ param, param)
             in M.elems $ o <> p
    structuralList env pParams
    structuralMaybe env $ tracedRequestBody <$> pc
    checkSubstructure env $ tracedResponses <$> pc
    checkSubstructure env $ do
      x <- pc
      se <- getH @(ProdCons [Server]) env
      pure $ Traced (ask x >>> step OperationServersStep) (getServers se (extract x))
    structuralList env $ fmap snd . tracedSecurity <$> pc
    -- TODO: Callbacks
    pure ()
  checkSemanticCompatibility env beh prodCons = do
    checkParameters
    checkRequestBodies
    checkResponses
    checkCallbacks
    checkOperationSecurity
    checkServers
    pure ()
    where
      checkParameters = do
        let -- Merged parameters got from Operation and PathItem in one
            -- place. First element is path params, second is non-path params
            tracedParams :: ProdCons ([Traced Param], [Traced Param])
            tracedParams = getParams <$> paramDefs <*> prodCons
            getParams defs mp =
              let operationParamsMap :: Map ParamKey (Traced Param)
                  operationParamsMap = M.fromList $ do
                    paramRef <- tracedParameters mp
                    let param = dereference defs paramRef
                        key = paramKey . extract $ param
                    pure (key, param)
                  pathParamsMap :: Map ParamKey (Traced Param)
                  pathParamsMap = M.fromList $ do
                    param <- pathParams . extract $ mp
                    pure (paramKey . extract $ param, param)
                  params = M.elems $ M.union operationParamsMap pathParamsMap -- We prefer params from Operation
                  splitted =
                    L.partition
                      (\p -> (_paramIn . extract $ p) == ParamPath)
                      params
               in splitted
        checkNonPathParams $ snd <$> tracedParams
        checkPathParams $ fst <$> tracedParams
        pure ()
      checkNonPathParams :: ProdCons [Traced Param] -> SemanticCompatFormula ()
      checkNonPathParams params = do
        let elements = getEls <$> params
            getEls params = M.fromList $ do
              p <- params
              let k = (_paramIn . extract $ p, _paramName . extract $ p)
                  v =
                    ProductLike
                      { productValue = p
                      , required = fromMaybe False . _paramRequired . extract $ p
                      }
              pure (k, v)
            check (_, name) param =
              checkCompatibility @Param (beh >>> step (InParam name)) env param
        checkProducts beh (ParamNotMatched . snd) check elements
      checkPathParams :: ProdCons [Traced Param] -> SemanticCompatFormula ()
      checkPathParams pathParams = do
        let fragments :: ProdCons [Traced PathFragmentParam]
            fragments = getFragments <$> pathParams <*> prodCons
            getFragments params mop = getPathFragments (extract mop) params
            -- Feed path parameters to the fragments getter
            check _ frags@(ProdCons (Traced _ p) _) =
              checkCompatibility @PathFragmentParam (beh >>> step (InFragment $ _paramName . extract <$> p)) env frags
            elements =
              fragments <&> \frags -> M.fromList $
                zip [0 :: Int ..] $ do
                  frag <- frags
                  pure $
                    ProductLike
                      { productValue = frag
                      , required = True
                      }
        checkProducts beh PathFragmentNotMatched check elements
      checkRequestBodies = do
        let check reqBody = checkCompatibility @RequestBody (beh >>> step InRequest) env reqBody
            elements = getReqBody <$> bodyDefs <*> prodCons
            getReqBody bodyDef mop = M.fromList $ do
              bodyRef <- F.toList . tracedRequestBody $ mop
              let body = dereference bodyDef bodyRef
              -- Single element map
              pure
                ( ()
                , ProductLike
                    { productValue = body
                    , required = fromMaybe False . _requestBodyRequired . extract $ body
                    }
                )
        checkProducts beh (const NoRequestBody) (const check) elements
      checkResponses =
        swapProdCons (checkCompatibility beh) env $ tracedResponses <$> prodCons
      -- FIXME: #27
      checkCallbacks = do
        let ProdCons pCallbacks cCallbacks = tracedCallbacks <$> prodCons
        for_ pCallbacks $ \(k, pCallback) -> do
          let beh' = beh >>> step (OperationCallback k)
          anyOfAt beh' CallbacksUnsupported $
            cCallbacks <&> \(_, cCallback) -> do
              swapProdCons (checkCompatibility beh') env $ ProdCons pCallback cCallback
        pure ()
      -- FIXME: #28
      checkOperationSecurity = do
        let ProdCons pSecs cSecs = tracedSecurity <$> prodCons
        for_ pSecs $ \(i, pSec) -> do
          let beh' = beh >>> step (SecurityRequirementStep i)
          anyOfAt beh' SecurityRequirementNotMet $
            cSecs <&> \(_, cSec) ->
              checkCompatibility beh' env $ ProdCons pSec cSec
      checkServers =
        checkCompatibility beh env $ do
          x <- prodCons
          se <- getH @(ProdCons [Server]) env
          pure $ Traced (ask x >>> step OperationServersStep) (getServers se (extract x))
      bodyDefs = getH @(ProdCons (Traced (Definitions RequestBody))) env
      paramDefs = getH @(ProdCons (Traced (Definitions Param))) env

data OperationMethod
  = GetMethod
  | PutMethod
  | PostMethod
  | DeleteMethod
  | OptionsMethod
  | HeadMethod
  | PatchMethod
  | TraceMethod
  deriving stock (Eq, Ord, Show)

pathItemMethod :: OperationMethod -> PathItem -> Maybe Operation
pathItemMethod = \case
  GetMethod -> _pathItemGet
  PutMethod -> _pathItemPut
  PostMethod -> _pathItemPost
  DeleteMethod -> _pathItemDelete
  OptionsMethod -> _pathItemOptions
  HeadMethod -> _pathItemHead
  PatchMethod -> _pathItemPatch
  TraceMethod -> _pathItemTrace

instance Steppable MatchedOperation (Referenced Param) where
  data Step MatchedOperation (Referenced Param) = OperationParamsStep Int
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation (Referenced RequestBody) where
  data Step MatchedOperation (Referenced RequestBody) = OperationRequestBodyStep
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation Responses where
  data Step MatchedOperation Responses = OperationResponsesStep
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation SecurityRequirement where
  data Step MatchedOperation SecurityRequirement = OperationSecurityRequirementStep Int
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation (Referenced Callback) where
  data Step MatchedOperation (Referenced Callback) = OperationCallbackStep Text
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedOperation [Server] where
  data Step MatchedOperation [Server]
    = OperationServersStep
    | EnvServerStep
    deriving stock (Eq, Ord, Show)

-- * ProcessedPathItems

-- FIXME: There's probably a better name for this, but `PathItem` is already taken ;(
data ProcessedPathItem = ProcessedPathItem
  { path :: FilePath
  , item :: PathItem
  }
  deriving stock (Eq, Show)

processPathItems :: [(FilePath, PathItem)] -> ProcessedPathItems
processPathItems = ProcessedPathItems . fmap (uncurry ProcessedPathItem)

newtype ProcessedPathItems = ProcessedPathItems {unProcessedPathItems :: [ProcessedPathItem]}
  deriving newtype (Eq, Show)

instance Issuable 'APILevel where
  data Issue 'APILevel
    = NoPathsMatched FilePath
    | AllPathsFailed FilePath
    -- When several paths match given but all checks failed
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    _ -> CertainIssue
  relatedIssues =
    (==) `withClass` \case
      NoPathsMatched fp -> Just fp
      AllPathsFailed fp -> Just fp
  describeIssue Forward (NoPathsMatched p) = para $ "The path " <> (code . T.pack) p <> " has been removed."
  describeIssue Backward (NoPathsMatched p) = para $ "The path " <> (code . T.pack) p <> " has been added."
  describeIssue Forward (AllPathsFailed p) = para $ "The path " <> (code . T.pack) p <> " has been removed."
  describeIssue Backward (AllPathsFailed p) = para $ "The path " <> (code . T.pack) p <> " has been added."

instance Behavable 'APILevel 'PathLevel where
  data Behave 'APILevel 'PathLevel
    = AtPath FilePath
    deriving stock (Eq, Ord, Show)

  describeBehavior (AtPath p) = str (T.pack p)

instance Subtree ProcessedPathItems where
  type SubtreeLevel ProcessedPathItems = 'APILevel
  type
    CheckEnv ProcessedPathItems =
      '[ ProdCons (Traced (Definitions Param))
       , ProdCons (Traced (Definitions RequestBody))
       , ProdCons (Traced (Definitions SecurityScheme))
       , ProdCons (Traced (Definitions Response))
       , ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons [Server]
       , ProdCons (Traced (Definitions Link))
       , ProdCons (Traced (Definitions Callback))
       ]

  -- No real way to check it at this level
  checkStructuralCompatibility _ _ = structuralIssue
  checkSemanticCompatibility env beh pc@(ProdCons p c) = do
    -- Each path generated by producer must be handled by consumer with exactly
    -- one way
    for_ (unProcessedPathItems . extract $ p) $ \prodItem -> do
      let prodPath = path prodItem
          beh' = beh >>> step (AtPath prodPath)
          matchedItems = do
            consItem <- unProcessedPathItems . extract $ c
            F.toList $ matchingPathItems $ ProdCons prodItem consItem
      case matchedItems of
        [] -> issueAt beh $ NoPathsMatched prodPath
        [match] -> checkCompatibility beh' env (retraced <$> pc <*> match)
        matches -> anyOfAt beh (AllPathsFailed prodPath) $ do
          match <- matches
          pure $ checkCompatibility beh' env (retraced <$> pc <*> match)
    where
      retraced pc mpi = traced (ask pc >>> step (MatchedPathStep $ matchedPath mpi)) mpi

-- | Preliminary checks two paths for compatibility.  Returns Nothing if two
-- paths obviously do not match: static parts differ or count of path elements
-- is not equal
matchingPathItems :: ProdCons ProcessedPathItem -> Maybe (ProdCons MatchedPathItem)
matchingPathItems prodCons = do
  let frags = parsePath . path <$> prodCons
  guard $ fragsMatch frags
  let mkMatchedItems frag ppi =
        MatchedPathItem
          { pathItem = item ppi
          , matchedPath = path ppi
          , pathFragments = frag
          }
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
  , -- | Pre-parsed path from PathItem
    pathFragments :: ![PathFragment Text]
  }
  deriving stock (Eq)

tracedMatchedPathItemParameters :: Traced MatchedPathItem -> [Traced (Referenced Param)]
tracedMatchedPathItemParameters mpi =
  [ traced (ask mpi >>> step (PathItemParam i)) x
  | (i, x) <- L.zip [0 ..] $ _pathItemParameters . pathItem $ extract mpi
  ]

tracedFragments :: Traced MatchedPathItem -> [Env (Trace PathFragmentParam) (PathFragment Text)]
tracedFragments mpi =
  [ env (ask mpi >>> step (PathFragmentStep i)) x
  | (i, x) <- L.zip [0 ..] $ pathFragments $ extract mpi
  ]

tracedMethod ::
  OperationMethod ->
  Traced MatchedPathItem ->
  Maybe (Traced' MatchedOperation Operation)
tracedMethod s mpi = env (ask mpi >>> step (OperationMethodStep s)) <$> (pathItemMethod s . pathItem . extract $ mpi)

instance Issuable 'PathLevel where
  data Issue 'PathLevel
    = OperationMissing OperationMethod
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    OperationMissing _ -> CertainIssue
  describeIssue Forward (OperationMissing op) = para $ "Method " <> strong (showMethod op) <> " has been removed."
  describeIssue Backward (OperationMissing op) = para $ "Method " <> strong (showMethod op) <> " has been added."

instance Behavable 'PathLevel 'OperationLevel where
  data Behave 'PathLevel 'OperationLevel
    = InOperation OperationMethod
    deriving stock (Eq, Ord, Show)

  describeBehavior (InOperation method) = showMethod method

showMethod :: IsString s => OperationMethod -> s
showMethod = \case
  GetMethod -> "GET"
  PutMethod -> "PUT"
  PostMethod -> "POST"
  DeleteMethod -> "DELETE"
  OptionsMethod -> "OPTIONS"
  HeadMethod -> "HEAD"
  PatchMethod -> "PATCH"
  TraceMethod -> "TRACE"

instance Subtree MatchedPathItem where
  type SubtreeLevel MatchedPathItem = 'PathLevel
  type
    CheckEnv MatchedPathItem =
      '[ ProdCons (Traced (Definitions Param))
       , ProdCons (Traced (Definitions RequestBody))
       , ProdCons (Traced (Definitions SecurityScheme))
       , ProdCons (Traced (Definitions Response))
       , ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons [Server]
       , ProdCons (Traced (Definitions Link))
       , ProdCons (Traced (Definitions Callback))
       ]
  checkStructuralCompatibility _ _ = structuralIssue
  checkSemanticCompatibility env beh prodCons = do
    let paramDefs = getH @(ProdCons (Traced (Definitions Param))) env
        pathTracedParams = getPathParams <$> paramDefs <*> prodCons
        getPathParams ::
          Traced (Definitions Param) ->
          Traced MatchedPathItem ->
          [Traced Param]
        getPathParams defs mpi = do
          paramRef <- tracedMatchedPathItemParameters mpi
          pure $ dereference defs paramRef
        pathTracedFragments = mkPathFragments <$> prodCons
        mkPathFragments mpi operationParams =
          --  operationParams will be known on Operation check stage, so we give a
          --  function, returning fragments
          let paramsMap :: Map Text (Traced Param)
              paramsMap = M.fromList $ do
                tracedParam <- operationParams
                let pname = _paramName . extract $ tracedParam
                pure (pname, tracedParam)
              convertFragment = \case
                StaticPath t -> StaticPath t
                DynamicPath pname ->
                  DynamicPath $
                    fromMaybe (error $ "Param not found " <> T.unpack pname) $
                      M.lookup pname paramsMap
           in tracedFragments mpi <&> fmap convertFragment
        operations = getOperations <$> pathTracedParams <*> pathTracedFragments <*> prodCons
        getOperations pathParams getPathFragments mpi = M.fromList $ do
          (name, getOp) <-
            (id &&& tracedMethod)
              <$> [GetMethod, PutMethod, PostMethod, DeleteMethod, OptionsMethod, HeadMethod, PatchMethod, DeleteMethod]
          operation <- F.toList $ getOp mpi
          -- Got only Justs here
          let retraced = \op -> MatchedOperation {operation = op, pathParams, getPathFragments}
          pure (name, retraced <$> operation)
        check name pc = checkCompatibility @MatchedOperation (beh >>> step (InOperation name)) env pc
    -- Operations are sum-like entities. Use step to operation as key because
    -- why not
    checkSums beh OperationMissing check operations

instance Steppable ProcessedPathItems MatchedPathItem where
  data Step ProcessedPathItems MatchedPathItem = MatchedPathStep FilePath
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedPathItem MatchedOperation where
  data Step MatchedPathItem MatchedOperation = OperationMethodStep OperationMethod
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedPathItem (Referenced Param) where
  data Step MatchedPathItem (Referenced Param) = PathItemParam Int
    deriving stock (Eq, Ord, Show)

instance Steppable MatchedPathItem PathFragmentParam where
  data Step MatchedPathItem PathFragmentParam = PathFragmentStep Int
    deriving stock (Eq, Ord, Show)

-- * Callbacks

instance Subtree Callback where
  type SubtreeLevel Callback = 'CallbackLevel
  type
    CheckEnv Callback =
      '[ ProdCons (Traced (Definitions Param))
       , ProdCons (Traced (Definitions RequestBody))
       , ProdCons (Traced (Definitions SecurityScheme))
       , ProdCons (Traced (Definitions Response))
       , ProdCons (Traced (Definitions Header))
       , ProdCons (Traced (Definitions Schema))
       , ProdCons (Traced (Definitions Link))
       , ProdCons [Server]
       , ProdCons (Traced (Definitions Callback))
       ]
  checkStructuralCompatibility env pc =
    checkSubstructure env $ tracedCallbackPathItems <$> pc
  checkSemanticCompatibility _ bhv _ = issueAt bhv CallbacksUnsupported

instance Issuable 'CallbackLevel where
  data Issue 'CallbackLevel
    = CallbacksUnsupported
    deriving stock (Eq, Ord, Show)
  issueKind = \case
    CallbacksUnsupported -> Unsupported
  describeIssue _ CallbacksUnsupported = para "CompaREST does not currently support callbacks."

tracedCallbackPathItems :: Traced Callback -> Traced ProcessedPathItems
tracedCallbackPathItems (Traced t (Callback x)) =
  Traced (t >>> step CallbackPathsStep) (processPathItems . fmap (first T.unpack) . IOHM.toList $ x)

instance Steppable Callback ProcessedPathItems where
  data Step Callback ProcessedPathItems = CallbackPathsStep
    deriving stock (Eq, Ord, Show)

instance Behavable 'OperationLevel 'CallbackLevel where
  data Behave 'OperationLevel 'CallbackLevel = OperationCallback Text
    deriving stock (Eq, Ord, Show)

  describeBehavior (OperationCallback key) = "Operation " <> code key
