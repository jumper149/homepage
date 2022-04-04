module Homepage.Server where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Route

import Control.Monad
import Control.Monad.Base
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Servant.Server.Generic
import System.Posix.Signals

server :: (MonadBaseControlIdentity IO m, MonadBlog m, MonadConfigured m, MonadLogger m) => m ()
server = do
  logInfo "Configure warp."
  withPort <- setPort . fromEnum . configPort <$> configuration
  withShutdownHandler <- liftBaseWithIdentity $ \ runInIO ->
    pure $ setInstallShutdownHandler $ \ closeSocket -> do
      let catchOnceShutdown sig = CatchOnce $ do
            runInIO $ do
              logInfo $ "Received signal '" <> T.pack (show @Signal sig) <> "'."
              logWarn "Shutdown."
            closeSocket
      let installShutdownHandler sig = void $ installHandler sig (catchOnceShutdown sig) Nothing
      installShutdownHandler sigHUP
      installShutdownHandler sigINT
      installShutdownHandler sigTERM
  let settings = withShutdownHandler $ withPort defaultSettings

  logInfo "Start server."
  liftBaseWithIdentity $ \ runInIO ->
    runSettings settings $ genericServeT (liftBase . runInIO) routes
