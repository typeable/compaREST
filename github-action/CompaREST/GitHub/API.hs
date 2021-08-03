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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GitHub as GH
import GitHub.Extra

findComment :: Members '[GitHub, Reader Config] effs => Eff effs (Maybe GH.Comment)
findComment = do
  Config {..} <- ask
  comments <- sendGitHub $ GH.pullRequestCommentsR repoOwner repoName issue GH.FetchAll
  htmlComment <- getHTMLComment
  let tryStripPrefix :: GH.Comment -> Maybe GH.Comment
      tryStripPrefix c@GH.Comment {commentBody = (T.stripPrefix htmlComment -> Just b)} = Just $ c {GH.commentBody = b}
      tryStripPrefix _ = Nothing
  pure . V.headM $ V.mapMaybe tryStripPrefix comments

mapComment :: Members '[GitHub, Reader Config] effs => (Text -> Text) -> Eff effs ()
mapComment f = do
  findComment
    >>= traverse_
      ( \comment -> do
          Config {..} <- ask
          htmlComment <- getHTMLComment
          sendGitHub $ editPullCommentR repoOwner repoName (GH.commentId comment) ((<> htmlComment) . f $ GH.commentBody comment)
          pure ()
      )

createOrUpdateComment :: Members '[GitHub, Reader Config] effs => Text -> Eff effs ()
createOrUpdateComment body' = do
  Config {..} <- ask
  htmlComment <- getHTMLComment
  let body = body' <> htmlComment
  void $
    findComment >>= \case
      Just comment -> sendGitHub $ editPullCommentR repoOwner repoName (GH.commentId comment) body
      Nothing -> sendGitHub $ createPullCommentSimpleR repoOwner repoName issue body

getHTMLComment :: Member (Reader Config) effs => Eff effs Text
getHTMLComment = do
  name <- asks projectName
  pure $ "\n\n<!-- compaREST comment – " <> name <> " -->"
