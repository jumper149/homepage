module Homepage.Server.Route.Static where

import Homepage.Configuration
import Homepage.Server.Err404

import Servant hiding (serveDirectoryWith)
import Servant.RawM.Server
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = RawM

handler :: MonadConfigured m => ServerT API m
handler = do
  path <- configDirectoryStatic <$> configuration
  serveDirectoryWith (defaultFileServerSettings path) { ss404Handler = Just application404 }
