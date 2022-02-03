module CompaREST.GitHub.API
  ( postStatus,
    postStatusProcessing,
  )
where

import CompaREST.GitHub.Action.Config
import Control.Monad.Freer
import Control.Monad.Freer.GitHub
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.OpenApi.Compare.Report
import Data.Proxy
import Data.Text (Text)
import GitHub
import GitHub.Data.Checks
import GitHub.Endpoints.Checks

postStatusProcessing ::
  (Members '[GitHub, Reader Config] effs, MonadIO (Eff effs)) =>
  Eff effs ()
postStatusProcessing = do
  Config {..} <- ask
  printJSON $
    sendGitHub $
      checkR
        repoOwner
        repoName
        Check
          { checkName = mkName Proxy checkName
          , checkSha = sha
          , checkDetailsURL = Nothing
          , checkExternalId = Nothing
          , checkStatus = Just CheckInProgress
          , checkStartedAt = Nothing
          , checkConclusion = Nothing
          , checkCompletedAt = Nothing
          , checkOutput = Nothing
          , checkActions = Nothing
          }

postStatus ::
  (Members '[GitHub, Reader Config] effs, MonadIO (Eff effs)) =>
  -- | 'Nothing' means that there were no changes at all
  Maybe (Text, ReportStatus) ->
  Eff effs ()
postStatus x = do
  let (body, (title, conclusion)) = case x of
        Just (b, s) -> (b,) $ case s of
          BreakingChanges -> ("⚠️ Breaking changes found!", CheckNeutral)
          NoBreakingChanges -> ("No breaking changes found ✨", CheckSuccess)
          OnlyUnsupportedChanges -> ("🤷 Couldn't determine compatibility", CheckNeutral)
        Nothing -> ("", ("✅ The API did not change", CheckSuccess))
  Config {..} <- ask
  printJSON $
    sendGitHub $
      checkR
        repoOwner
        repoName
        Check
          { checkName = mkName Proxy checkName
          , checkSha = sha
          , checkDetailsURL = Nothing
          , checkExternalId = Nothing
          , checkStatus = Just CheckCompleted
          , checkStartedAt = Nothing
          , checkConclusion = Just conclusion
          , checkCompletedAt = Nothing
          , checkOutput =
              Just $
                CheckOutput
                  { checkTitle = title
                  , checkSummary = body
                  , checkText = Nothing
                  , checkAnnotations = Nothing
                  , checkImages = Nothing
                  }
          , checkActions = Nothing
          }

printJSON :: MonadIO (Eff effs) => Eff effs Value -> Eff effs ()
printJSON m = do
  x <- m
  liftIO . BSLC.putStrLn $ encode x
