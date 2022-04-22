module Homepage.Server where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.BaseUrl
import Homepage.Handler
import Homepage.Server.Route

import Control.Monad
import Control.Monad.Base
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Data.ByteString.Char8 qualified as B
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Trans
import Servant.Server
import Servant.Server.Generic
import System.Posix.Signals

server :: (MonadBaseControlIdentity IO m, MonadBlog m, MonadConfigured m, MonadLogger m) => m ()
server = do
  logInfo "Configure warp."
  withPort <- setPort . fromEnum . configPort <$> configuration
  withShutdownHandler <- liftBaseWithIdentity $ \runInIO ->
    pure . setInstallShutdownHandler $ \closeSocket -> do
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
  liftBaseWithIdentity $ \runInIO ->
    runSettings settings . addMiddleware $
      serveWithContextT (Proxy @WrappedAPI) EmptyContext (liftBase . runInIO) $
        hoistServerRunHandlerT $ genericServerT routes

middleware :: (MonadConfigured m, MonadLogger m) => MiddlewareT m
middleware application req resp = do
  logDebug $ "Received HTTP request: " <> T.pack (show req)
  let path = T.decodeLatin1 $ rawPathInfo req
      found302Builder locationPath = do
        baseUrl <- configBaseUrl <$> configuration
        let location = B.pack (T.unpack $ displayBaseUrlPath baseUrl <> locationPath) <> rawQueryString req
        logInfo $ "Redirect HTTP request to new location: " <> T.pack (show location)
        resp $ responseBuilder status302 [(hLocation, location)] mempty
  case T.unsnoc path of
    Nothing -> found302Builder "/"
    Just ("", '/') -> application req resp
    Just (xs, '/') -> found302Builder xs
    _ -> application req resp
