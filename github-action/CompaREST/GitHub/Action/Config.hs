module CompaREST.GitHub.Action.Config
  ( Config (..),
  )
where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified GitHub as GH

import Data.Functor
import System.Envy

data Config = Config
  { githubToken :: GH.Auth
  , repoOwner :: GH.Name GH.Owner
  , repoName :: GH.Name GH.Repo
  , checkName :: Text
  , footerText :: Text
  , root :: FilePath
  , sha :: GH.Name GH.Commit
  }

instance FromEnv Config where
  fromEnv _ = do
    token <- GH.OAuth <$> env "GITHUB_TOKEN"
    (owner, repo) <-
      T.split (== '/') <$> env "REPO" >>= \case
        [owner, name] -> pure (owner, name)
        _ -> fail "malformed repo"
    checkName <-
      envMaybe "PROJECT_NAME" <&> \case
        Just pName | not . T.null . T.strip $ pName -> "compaREST – " <> pName
        _ -> "compaREST"
    footerText <- env "FOOTER"
    root <- envMaybe "ROOT" .!= "."
    sha <- env "SHA"
    pure $
      Config
        { githubToken = token
        , repoOwner = GH.mkName Proxy owner
        , repoName = GH.mkName Proxy repo
        , checkName = checkName
        , footerText = footerText
        , root = root
        , sha = GH.mkName Proxy sha
        }
