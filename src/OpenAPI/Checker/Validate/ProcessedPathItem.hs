{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.ProcessedPathItem
  ( ProcessedPathItem (..)
  , ProcessedPathItems (..)
  , processPathItems
  , Step (..)
  )
where

import Data.Foldable
import Data.HList
import Data.Maybe
import Data.OpenApi
import Generic.Data
import OpenAPI.Checker.References
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Operation
import OpenAPI.Checker.Validate.PathFragment

-- FIXME: There's probably a better name for this, but `PathItem` is already taken ;(
data ProcessedPathItem = ProcessedPathItem
  { path :: FilePath
  , item :: PathItem
  }

processPathItems :: [(FilePath, PathItem)] -> ProcessedPathItems
processPathItems = ProcessedPathItems . fmap (uncurry ProcessedPathItem)

newtype ProcessedPathItems = ProcessedPathItems {unProcessedPathItems :: [ProcessedPathItem]}

instance Subtree ProcessedPathItems where
  type
    CheckEnv ProcessedPathItems =
      '[ ProdCons (Definitions Param)
       , ProdCons (Definitions RequestBody)
       , ProdCons (Definitions SecurityScheme)
       ]
  data CheckIssue ProcessedPathItems = NoPathsMatched | WrongNumberOfFragments
    deriving (Eq, Ord, Show)
  normalizeTrace = undefined
  checkCompatibility env prodCons = do
    let ProdCons {producer = p, consumer = c} =
          (\paramDefs -> fmap (processPathItem paramDefs) . unProcessedPathItems)
            <$> getH @(ProdCons (Definitions Param)) env
            <*> prodCons
    sequenceA_
      [ anyOfAt
        producer
        NoPathsMatched
        [ localTrace (step <$> ProdCons pSPath cSPath) $ do
          -- make sure every path fragment is compatible
          sequenceA_
            [ localTrace (pure . step $ PathFragmentStep i) $
              checkCompatibility (singletonH $ ProdCons pPathFragmentParams cPathFragmentParams) pair
            | (i, pair) <- zip [0 ..] pathFragments
            ]
          -- make sure the operation is compatible.
          localTrace (pure . step $ getter stepProcessedPathItem) $
            checkCompatibility env $ ProdCons pOperation cOperation
          pure ()
        | (cSPath, cPath, cPathItem) <- c
        , -- ... and try to match it with every endpoint in the consumer.
        --
        -- This is required because the meaning of path fragments can change on
        -- a per-method basis even within the same 'PathItem'
        --
        -- Here we only need to look for the method that the current producer
        -- endpoint is using.
        (cParams, cOperation) <- maybeToList $ getter cPathItem
        , let cPathFragmentParams = retrace (step PathFragmentParentStep >>>) <$> cParams
        , -- make sure the paths are the same length
        pathFragments <- maybeToList $ zipAllWith ProdCons pPath cPath
        ]
      | (pSPath, pPath, pPathItem) <- p
      , -- look at every endpoint in the producer ...
      (ProcessedPathItemGetter getter, (pParams, pOperation)) <-
        toList (fmap . (,) <$> processedPathItemGetters <*> pPathItem) >>= maybeToList
      , let pPathFragmentParams = retrace (step PathFragmentParentStep >>>) <$> pParams
      ]

zipAllWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipAllWith _ [] [] = Just []
zipAllWith f (x : xs) (y : ys) = (f x y :) <$> zipAllWith f xs ys
zipAllWith _ (_ : _) [] = Nothing
zipAllWith _ [] (_ : _) = Nothing

processPathItem
  :: Definitions Param -- ^ from components
  -> ProcessedPathItem
  -> ( Step ProcessedPathItems PathItem
     , [PathFragment]
     , ForeachOperation (Maybe (TracedReferences PathItem Param, Operation))
     )
processPathItem componentParams ProcessedPathItem {path = pathS, item = pathItem} =
  let path = parsePath pathS
      commonPathParams =
        retrace (step PathItemParametersStep >>>)
          <$> getPathParamRefs componentParams (_pathItemParameters pathItem)
      processOperation (s :: Step PathItem Operation) op =
        let operationParams =
              retrace (Root `Snoc` s `Snoc` OperationParamsStep >>>)
                <$> getPathParamRefs componentParams (_operationParameters op)
            pathParams =
              operationParams <> commonPathParams
         in (pathParams, op)
   in ( PathStep pathS
      , path
      , fmap . processOperation
          <$> stepProcessedPathItem
          <*> ForeachOperation
            { processedPathItemGet = _pathItemGet pathItem
            , processedPathItemPut = _pathItemPut pathItem
            , processedPathItemPost = _pathItemPost pathItem
            , processedPathItemDelete = _pathItemDelete pathItem
            , processedPathItemOptions = _pathItemOptions pathItem
            , processedPathItemHead = _pathItemHead pathItem
            , processedPathItemPatch = _pathItemPatch pathItem
            , processedPathItemTrace = _pathItemTrace pathItem
            }
      )

instance Steppable ProcessedPathItems PathItem where
  data Step ProcessedPathItems PathItem = PathStep FilePath
    deriving (Eq, Ord, Show)

instance Steppable PathItem PathFragment where
  data Step PathItem PathFragment
    = -- | The index of the path item
      PathFragmentStep Int
    deriving (Eq, Ord, Show)

instance Steppable PathFragment PathItem where
  data Step PathFragment PathItem = PathFragmentParentStep
    deriving (Eq, Ord, Show)

instance Steppable PathItem (Referenced Param) where
  data Step PathItem (Referenced Param) = PathItemParametersStep
    deriving (Eq, Ord, Show)

data ForeachOperation a = ForeachOperation
  { processedPathItemGet :: a
  , processedPathItemPut :: a
  , processedPathItemPost :: a
  , processedPathItemDelete :: a
  , processedPathItemOptions :: a
  , processedPathItemHead :: a
  , processedPathItemPatch :: a
  , processedPathItemTrace :: a
  }
  deriving stock (Functor, Generic1)
  deriving (Applicative, Foldable) via Generically1 ForeachOperation

newtype ProcessedPathItemGetter = ProcessedPathItemGetter (forall a. ForeachOperation a -> a)

processedPathItemGetters :: ForeachOperation ProcessedPathItemGetter
processedPathItemGetters =
  ForeachOperation
    { processedPathItemGet = ProcessedPathItemGetter processedPathItemGet
    , processedPathItemPut = ProcessedPathItemGetter processedPathItemPut
    , processedPathItemPost = ProcessedPathItemGetter processedPathItemPost
    , processedPathItemDelete = ProcessedPathItemGetter processedPathItemDelete
    , processedPathItemOptions = ProcessedPathItemGetter processedPathItemOptions
    , processedPathItemHead = ProcessedPathItemGetter processedPathItemHead
    , processedPathItemPatch = ProcessedPathItemGetter processedPathItemPatch
    , processedPathItemTrace = ProcessedPathItemGetter processedPathItemTrace
    }

stepProcessedPathItem :: ForeachOperation (Step PathItem Operation)
stepProcessedPathItem =
  ForeachOperation
    { processedPathItemGet = GetStep
    , processedPathItemPut = PutStep
    , processedPathItemPost = PostStep
    , processedPathItemDelete = DeleteStep
    , processedPathItemOptions = OptionsStep
    , processedPathItemHead = HeadStep
    , processedPathItemPatch = PatchStep
    , processedPathItemTrace = TraceStep
    }
