{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server where

import Homepage.Application
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Route

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Network.Wai.Handler.Warp
import Servant.Server.Generic
import System.Posix.Signals

server :: ApplicationT IO ()
server = do
  $logInfo "Configure warp."
  withPort <- setPort . fromEnum . configPort <$> configuration
  withShutdownHandler <- liftBaseWith $ \ runInBase ->
    pure $ setInstallShutdownHandler $ \ closeSocket -> do
      let catchOnceShutdown sig = CatchOnce $ do
            void $ runInBase $ do
              $logInfo $ "Received signal '" <> T.pack (show sig) <> "'."
              $logWarn "Shutdown."
            closeSocket
      let installShutdownHandler sig = void $ installHandler sig (catchOnceShutdown sig) Nothing
      installShutdownHandler sigHUP
      installShutdownHandler sigINT
      installShutdownHandler sigTERM
  let settings = withShutdownHandler $ withPort defaultSettings

  $logInfo "Start server."
  liftWith $ \ runT ->
    runSettings settings $ genericServeT runT routes
