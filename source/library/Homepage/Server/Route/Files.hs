module Homepage.Server.Route.Files where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Err404
import Homepage.Server.FileServer
import Homepage.Server.Html.Document
import Homepage.Server.Html.Files
import Homepage.Server.Tab

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Network.Wai.Trans
import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM.Server qualified as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import WaiAppStatic.Types

data Routes route = Routes
  { routeOverview :: route :- Get '[HTML] Html
  , routeFiles :: route :- RawM.RawM
  }
  deriving stock (Generic)

routes ::
  (MonadBaseControlIdentity IO m, MonadConfigured m, MonadLogger m) =>
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
  (MonadBaseControlIdentity IO m, MonadConfigured m, MonadLogger m) =>
  ServerT RawM.RawM m
filesHandler = do
  directory <- configDirectoryFiles <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve file download."
  settings <- fileServerSettings directory
  RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
