module Control.Monad.Freer.GitHub
  ( GitHub (..),
    runGitHub,
    sendGitHub,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.IO.Class
import Data.Aeson
import GitHub hiding (Error)
import qualified GitHub as GH

data GitHub r where
  SendGHRequest :: FromJSON x => Request 'RW x -> GitHub x

runGitHub :: (Member (Error GH.Error) effs, MonadIO (Eff effs)) => Auth -> Eff (GitHub ': effs) ~> Eff effs
runGitHub auth =
  interpret
    ( \(SendGHRequest req) ->
        liftIO (executeRequest auth req)
          >>= either throwError pure
    )

sendGitHub :: (FromJSON x, Member GitHub effs) => Request 'RW x -> Eff effs x
sendGitHub req = send $ SendGHRequest req
