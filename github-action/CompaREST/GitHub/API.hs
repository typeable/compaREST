module CompaREST.GitHub.API
  ( mapComment,
    createOrUpdateComment,
  )
where

import CompaREST.GitHub.Action.Config
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.GitHub
import Control.Monad.Freer.Reader
import Data.Foldable
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import qualified GitHub as GH

findComment :: Members '[GitHub, Reader Config] effs => Eff effs (Maybe GH.IssueComment)
findComment = do
  Config {..} <- ask
  comments <- sendGitHub $ GH.commentsR repoOwner repoName issue GH.FetchAll
  htmlComment <- getHTMLComment
  let tryStripPrefix :: GH.IssueComment -> Maybe GH.IssueComment
      tryStripPrefix c@GH.IssueComment {issueCommentBody = (T.stripSuffix htmlComment -> Just b)} =
        Just $ c {GH.issueCommentBody = b}
      tryStripPrefix _ = Nothing
  pure . (V.!? 0) $ V.mapMaybe tryStripPrefix comments

mapComment :: Members '[GitHub, Reader Config] effs => (Text -> Text) -> Eff effs ()
mapComment f = do
  findComment
    >>= traverse_
      ( \comment -> do
          Config {..} <- ask
          htmlComment <- getHTMLComment
          sendGitHub $ editCommentR repoOwner repoName (GH.mkId Proxy $ GH.issueCommentId comment) ((<> htmlComment) . f $ GH.issueCommentBody comment)
          pure ()
      )

createOrUpdateComment :: Members '[GitHub, Reader Config] effs => Text -> Eff effs ()
createOrUpdateComment body' = do
  Config {..} <- ask
  htmlComment <- getHTMLComment
  let body = body' <> htmlComment
  void $
    findComment >>= \case
      Just comment -> sendGitHub $ editCommentR repoOwner repoName (GH.mkId Proxy $ GH.issueCommentId comment) body
      Nothing -> sendGitHub $ createCommentR repoOwner repoName issue body

getHTMLComment :: Member (Reader Config) effs => Eff effs Text
getHTMLComment = do
  name <- asks projectName
  pure $ "\n\n<!-- compaREST comment – " <> name <> " -->"
