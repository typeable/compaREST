module GitHub.Extra
  ( editPullCommentR,
    createPullCommentSimpleR,
  )
where

import Data.Aeson
import Data.Text (Text)
import GitHub

editPullCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request 'RW Comment
editPullCommentR user repo commid body =
  command Patch parts (encode $ EditComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "pulls", "comments", toPathPart commid]

createPullCommentSimpleR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request 'RW Comment
createPullCommentSimpleR user repo iss body =
  command Post parts (encode $ NewComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart iss, "comments"]
