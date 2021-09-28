{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application where

import Homepage.Application.Blog
import Homepage.Application.Compose
import Homepage.Application.Configurable
import Homepage.Application.Configured
import Homepage.Application.Logging
import Homepage.Configuration
import Homepage.Configuration.Acquisition

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Foldable

newtype ApplicationT m a = ApplicationT { unApplicationT :: (BlogT |. ConfiguredT |. LoggingT' |. ConfigurableT |. IdentityT) m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadThrow, MonadCatch)
  deriving newtype (MonadLogger)
  deriving newtype (MonadError e)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadBlog)

runApplication :: (MonadIO m, MonadBaseControl IO m)
               => ApplicationT m a
               -> m a
runApplication app = do
  (preConfig, preConfigLog) <- runWriterLoggingT acquirePreConfig

  let

    runConfigurableT' tma = runConfigurableT tma preConfig

    runLoggingT'' tma = do
      maybeLogFile <- preConfigLogFile <$> preConfiguration
      runLoggingT' maybeLogFile $ do
        traverse_ logDelayed preConfigLog
        tma

    runConfiguredT' tma = do
      maybeConfig <- acquireConfig =<< preConfiguration
      case maybeConfig of
        Nothing -> error "No configuration."
        Just config -> runConfiguredT tma config

  runBlogT |.| runConfiguredT' |.| runLoggingT'' |.| runConfigurableT' |.| runIdentityT $ unApplicationT app
