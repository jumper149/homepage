module Homepage.Server.Route.Static where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Err404

import Control.Monad.Logger.CallStack
import Servant
import Servant.RawM.Server qualified as RawM
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = RawM.RawM

handler :: (MonadConfigured m, MonadLogger m)
        => ServerT API m
handler = do
  path <- configDirectoryStatic <$> configuration
  fallbackApplication <- application404
  logInfo "Serve static file download."
  RawM.serveDirectoryWith (defaultFileServerSettings path) { ss404Handler = Just fallbackApplication }
