module Homepage.Server.Route.Files where

import Homepage.Configuration
import Homepage.Server.Err404

import Servant
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = Get '[JSON] String :<|> Raw

handler :: MonadConfigured m => ServerT API m
handler = pure "files"
     :<|> serveDirectoryWith (defaultFileServerSettings ".") { ss404Handler = Just application404 }
