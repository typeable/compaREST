module GitHub.Endpoints.Checks
  ( checkR,
  )
where

import Data.Aeson
import GitHub
import GitHub.Data.Checks

checkR :: Name Owner -> Name Repo -> Check -> Request 'RW ()
checkR user repo =
  command Post ["repos", toPathPart user, toPathPart repo, "check-runs"] . encode
