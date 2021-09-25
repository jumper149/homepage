module Homepage.Server.Route.Static where

import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Err404

import Servant
import qualified Servant.RawM.Server as RawM
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = RawM.RawM

handler :: MonadConfigured m => ServerT API m
handler = do
  path <- configDirectoryStatic <$> configuration
  RawM.serveDirectoryWith (defaultFileServerSettings path) { ss404Handler = Just application404 }
