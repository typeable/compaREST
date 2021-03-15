{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Checker.Validate.OpenApi
  (
  )
where

import Data.Foldable
import Data.HList
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.OpenApi
import Generic.Data
import OpenAPI.Checker.Subtree
import OpenAPI.Checker.Trace
import OpenAPI.Checker.Validate.Operation
import OpenAPI.Checker.Validate.PathFragment

instance Subtree OpenApi where
  type CheckEnv OpenApi = '[]
  data CheckIssue OpenApi
    deriving (Eq, Ord)
  normalizeTrace = undefined
  checkCompatibility env prodCons = do
    let ProdCons {producer = p, consumer = c} = processOpenApi <$> prodCons
    sequenceA_ @[] @_ @() $ do
      (path, x) <- p
      let f :: ProdCons (PathParamRefs, Operation) -> CompatFormula OpenApi ()
          f prodConsPaths = do
            localTrace undefined $ (checkCompatibility (singletonH $ fst <$> prodConsPaths) . pure) `traverse` path
            localTrace undefined $ checkCompatibility HNil $ snd <$> prodConsPaths
            pure ()
      -- checkCompatibility HNil undefined
      -- (pathS, pathItem) <- IOHM.toList . _openApiPaths $ p
      -- let path = parsePath pathS
      --     _ =
      --       getPathParamRefs (_componentsParameters cs `HCons` env) $
      --         _pathItemParameters pathItem
      pure $ do
        -- check
        pure ()
    -- [f ()] -> f ()
    pure ()

instance Steppable PathItem (Referenced Param) where
  data Step PathItem (Referenced Param) = PathItemParametersStep
    deriving (Eq, Ord)

instance Steppable Operation (Referenced Param) where
  data Step Operation (Referenced Param) = OperationParametersStep
    deriving (Eq, Ord)

processOpenApi ::
  OpenApi ->
  [ ( [PathFragment],
      ProcessedPathItem (Maybe (TracedReferences PathItem Param, Operation))
    )
  ]
processOpenApi o = do
  let cs = _openApiComponents o
  (pathS, pathItem) <- IOHM.toList . _openApiPaths $ o
  let path = parsePath pathS
      componentParamsEnv = singletonH $ _componentsParameters cs
      commonPathParams =
        fmap (retrace (step PathItemParametersStep >>>)) $
          getPathParamRefs componentParamsEnv $
            _pathItemParameters pathItem
      processOperation (s :: Step PathItem Operation) op =
        let operationParams =
              retrace (Root `Snoc` s `Snoc` OperationParametersStep >>>)
                <$> getPathParamRefs componentParamsEnv (_operationParameters op)
            pathParams =
              operationParams <> commonPathParams
         in (pathParams, op)
  pure
    ( path,
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
  { -- processedPath :: [PathFragment],
    processedPathItemGet :: a,
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
