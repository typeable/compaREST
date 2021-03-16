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
    sequenceA_ @[] @_ @() $ do
      (pSPath, pPath, pPathItem) <- p
      (ProcessedPathItemGetter getter, (pParams, pOperation)) <-
        toList (fmap . (,) <$> processedPathItemGetters <*> pPathItem) >>= maybeToList
      let pPathFragmentParams = retrace (step PathFragmentParentStep >>>) <$> pParams
      pure $ do
        anyOfF producer NoPathsMatched $ do
          (cSPath, cPath, cPathItem) <- c
          (cParams, cOperation) <- maybeToList $ getter cPathItem
          let cPathFragmentParams = retrace (step PathFragmentParentStep >>>) <$> cParams
          pathFragments <- maybeToList $ zipAllWith ProdCons pPath cPath
          pure . localTrace (step <$> ProdCons pSPath cSPath) $ do
            sequenceA_ $ do
              (i, pair) <- zip [0 ..] pathFragments
              pure . localTrace (pure . step $ PathFragmentStep i) $
                checkCompatibility (singletonH $ ProdCons pPathFragmentParams cPathFragmentParams) pair
            localTrace (pure . step $ getter stepProcessedPathItem) $
              checkCompatibility HNil $ ProdCons pOperation cOperation
            pure ()
    pure ()

zipAllWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipAllWith _ [] [] = Just []
zipAllWith f (x : xs) (y : ys) = (f x y :) <$> zipAllWith f xs ys
zipAllWith _ (_ : _) [] = Nothing
zipAllWith _ [] (_ : _) = Nothing

instance Steppable PathFragment PathItem where
  data Step PathFragment PathItem = PathFragmentParentStep
    deriving (Eq, Ord)

instance Steppable OpenApi PathItem where
  data Step OpenApi PathItem = PathStep FilePath
    deriving (Eq, Ord)

instance Steppable PathItem PathFragment where
  data Step PathItem PathFragment
    = -- | The index of the path item
      PathFragmentStep Int
    deriving (Eq, Ord)

instance Steppable PathItem (Referenced Param) where
  data Step PathItem (Referenced Param) = PathItemParametersStep
    deriving (Eq, Ord)

instance Steppable Operation (Referenced Param) where
  data Step Operation (Referenced Param) = OperationParametersStep
    deriving (Eq, Ord)

processOpenApi ::
  OpenApi ->
  [ ( Step OpenApi PathItem,
      [PathFragment],
      ProcessedPathItem (Maybe (TracedReferences PathItem Param, Operation))
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
    ( PathStep pathS,
      path,
      fmap . processOperation
        <$> stepProcessedPathItem
        <*> ProcessedPathItem
          { processedPathItemGet = _pathItemGet pathItem,
            processedPathItemPut = _pathItemPut pathItem,
            processedPathItemPost = _pathItemPost pathItem,
            processedPathItemDelete = _pathItemDelete pathItem,
            processedPathItemOptions = _pathItemOptions pathItem,
            processedPathItemHead = _pathItemHead pathItem,
            processedPathItemPatch = _pathItemPatch pathItem,
            processedPathItemTrace = _pathItemTrace pathItem
          }
    )

data ProcessedPathItem a = ProcessedPathItem
  { processedPathItemGet :: a,
    processedPathItemPut :: a,
    processedPathItemPost :: a,
    processedPathItemDelete :: a,
    processedPathItemOptions :: a,
    processedPathItemHead :: a,
    processedPathItemPatch :: a,
    processedPathItemTrace :: a
  }
  deriving stock (Functor, Generic1)
  deriving (Applicative, Foldable) via Generically1 ProcessedPathItem

newtype ProcessedPathItemGetter = ProcessedPathItemGetter (forall a. ProcessedPathItem a -> a)

processedPathItemGetters :: ProcessedPathItem ProcessedPathItemGetter
processedPathItemGetters =
  ProcessedPathItem
    { processedPathItemGet = ProcessedPathItemGetter processedPathItemGet,
      processedPathItemPut = ProcessedPathItemGetter processedPathItemPut,
      processedPathItemPost = ProcessedPathItemGetter processedPathItemPost,
      processedPathItemDelete = ProcessedPathItemGetter processedPathItemDelete,
      processedPathItemOptions = ProcessedPathItemGetter processedPathItemOptions,
      processedPathItemHead = ProcessedPathItemGetter processedPathItemHead,
      processedPathItemPatch = ProcessedPathItemGetter processedPathItemPatch,
      processedPathItemTrace = ProcessedPathItemGetter processedPathItemTrace
    }

stepProcessedPathItem :: ProcessedPathItem (Step PathItem Operation)
stepProcessedPathItem =
  ProcessedPathItem
    { processedPathItemGet = GetStep,
      processedPathItemPut = PutStep,
      processedPathItemPost = PostStep,
      processedPathItemDelete = DeleteStep,
      processedPathItemOptions = OptionsStep,
      processedPathItemHead = HeadStep,
      processedPathItemPatch = PatchStep,
      processedPathItemTrace = TraceStep
    }

impliesOrElse ::
  Subtree t =>
  (forall x. ProdCons x -> x) ->
  CheckIssue t ->
  (a -> b -> CompatFormula t ()) ->
  Maybe a ->
  Maybe b ->
  CompatFormula t ()
impliesOrElse f issue _ (Just _) Nothing = issueAt f issue
impliesOrElse _ _ act (Just a) (Just b) = act a b
impliesOrElse _ _ _ Nothing _ = pure ()
