module Homepage.Server where

import Homepage.Application
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Route

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Servant.Server.Generic
import System.Posix.Signals

server :: MonadIO m => ApplicationT m ()
server = do
  logInfo "Configure warp."
  withPort <- setPort . fromEnum . configPort <$> configuration
  withShutdownHandler <- liftWith $ \ runT ->
    pure $ setInstallShutdownHandler $ \ closeSocket -> do
      let catchOnceShutdown sig = CatchOnce $ do
            void $ runT $ do
              logInfo $ "Received signal '" <> T.pack (show sig) <> "'."
              logWarn "Shutdown."
            closeSocket
      let installShutdownHandler sig = void $ installHandler sig (catchOnceShutdown sig) Nothing
      installShutdownHandler sigHUP
      installShutdownHandler sigINT
      installShutdownHandler sigTERM
  let settings = withShutdownHandler $ withPort defaultSettings

  logInfo "Start server."
  liftWith $ \ runT ->
    liftIO $ runSettings settings $ genericServeT runT routes
