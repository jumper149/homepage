module Homepage.Server where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Handler
import Homepage.Handler.RequestHash
import Homepage.Server.Route

import Control.Monad
import Control.Monad.Base
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Data.Proxy
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Network.Wai.Trans
import Servant.Server
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

  logInfo "Configure middleware."
  addMiddleware <- runMiddlewareT middleware

  logInfo "Start server."
  liftBaseWithIdentity $ \ runInIO ->
    runSettings settings $ addMiddleware $
      serveWithContextT (Proxy @WrappedAPI) EmptyContext (liftBase . runInIO) $
        wrapApi $ genericServerT routes

middleware :: MonadLogger m => MiddlewareT m
middleware application req resp = do
  let reqHash = T.pack $ showHash $ requestHash req
  logInfo $ "Received HTTP request: " <> reqHash
  logDebug $ "Full HTTP request '" <> reqHash <> "': " <> T.pack (show req)
  application req resp
