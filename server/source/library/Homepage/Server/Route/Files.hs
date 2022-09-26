module Homepage.Server.Route.Files where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Err404
import Homepage.Server.FileServer
import Homepage.Server.Html.Document
import Homepage.Server.Html.Files
import Homepage.Server.Route.Files.Type
import Homepage.Server.Tab

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Network.Wai.Trans
import Servant
import Servant.RawM.Server qualified as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import WaiAppStatic.Types

routes ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeOverview = overviewHandler
    , routeFiles = filesHandler
    }

overviewHandler ::
  (MonadConfigured m, MonadLogger m) =>
  m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  fileEntries <- configFileEntries <$> configuration
  logInfo "Serve files overview."
  pure . document baseUrl contactInformation revision (Just 0) (Just TabFiles) $ do
    h2 "my Files"
    fileList baseUrl (Just 0) fileEntries

filesHandler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT RawM.RawM m
filesHandler = do
  directory <- configDirectoryFiles <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve file download."
  settings <- fileServerSettings directory
  RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
