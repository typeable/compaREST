module CompaREST.GitHub.Action.Config
  ( Config (..),
  )
where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified GitHub as GH

import System.Envy

data Config = Config
  { githubToken :: GH.Auth
  , repoOwner :: GH.Name GH.Owner
  , repoName :: GH.Name GH.Repo
  , issue :: GH.IssueNumber
  , projectName :: Text
  , footerText :: Text
  , root :: FilePath
  }

instance FromEnv Config where
  fromEnv _ = do
    token <- GH.OAuth <$> env "GITHUB_TOKEN"
    (owner, repo) <-
      T.split (== '/') <$> env "REPO" >>= \case
        [owner, name] -> pure (owner, name)
        _ -> fail "malformed repo"
    issue <- GH.IssueNumber <$> env "PR_NUMBER"
    projectName <- env "PROJECT_NAME"
    footerText <- env "FOOTER"
    root <- envMaybe "ROOT" .!= "."
    pure $
      Config
        { githubToken = token
        , repoOwner = GH.mkName Proxy owner
        , repoName = GH.mkName Proxy repo
        , issue = issue
        , projectName = projectName
        , footerText = footerText
        , root = root
        }
