{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.OpenApi
  (
  )
where

import Data.Foldable
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe
import Data.OpenApi
import Generic.Data
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Operation
import OpenAPI.Checker.Validate.PathFragment

instance Subtree OpenApi where
  type CheckEnv OpenApi = '[]
  data CheckIssue OpenApi = NoPathsMatched | WrongNumberOfFragments
    deriving (Eq, Ord)
  normalizeTrace = undefined
  checkCompatibility _ prodCons = do
    let ProdCons {producer = p, consumer = c} = processOpenApi <$> prodCons
    sequenceA_ @[] @_ @()
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
            checkCompatibility HNil $ ProdCons pOperation cOperation
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

processOpenApi
  :: OpenApi
  -> [ ( Step OpenApi PathItem
       , [PathFragment]
       , ForeachOperation (Maybe (TracedReferences PathItem Param, Operation))
       )
     ]
processOpenApi o = do
  let cs = _openApiComponents o
  (pathS, pathItem) <- IOHM.toList . _openApiPaths $ o
  let path = parsePath pathS
      componentParamsEnv = singletonH $ _componentsParameters cs
      commonPathParams =
        retrace (step PathItemParametersStep >>>)
          <$> getPathParamRefs componentParamsEnv (_pathItemParameters pathItem)
      processOperation (s :: Step PathItem Operation) op =
        let operationParams =
              retrace (Root `Snoc` s `Snoc` OperationParametersStep >>>)
                <$> getPathParamRefs componentParamsEnv (_operationParameters op)
            pathParams =
              operationParams <> commonPathParams
         in (pathParams, op)
  pure
    ( PathStep pathS
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

instance Steppable OpenApi PathItem where
  data Step OpenApi PathItem = PathStep FilePath
    deriving (Eq, Ord)

instance Steppable PathItem PathFragment where
  data Step PathItem PathFragment
    = -- | The index of the path item
      PathFragmentStep Int
    deriving (Eq, Ord)

instance Steppable PathFragment PathItem where
  data Step PathFragment PathItem = PathFragmentParentStep
    deriving (Eq, Ord)

instance Steppable PathItem (Referenced Param) where
  data Step PathItem (Referenced Param) = PathItemParametersStep
    deriving (Eq, Ord)

instance Steppable Operation (Referenced Param) where
  data Step Operation (Referenced Param) = OperationParametersStep
    deriving (Eq, Ord)

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
