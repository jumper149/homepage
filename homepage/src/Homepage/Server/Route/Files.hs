module Homepage.Server.Route.Files where

import Homepage.Configuration
import Homepage.Server.Err404

import Servant
import qualified Servant.RawM.Server as RawM
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = RawM.RawM

handler :: MonadConfigured m => ServerT API m
handler = do
  dir <- configDirectoryFiles <$> configuration
  RawM.serveDirectoryWith (defaultFileServerSettings dir) { ss404Handler = Just application404 }
