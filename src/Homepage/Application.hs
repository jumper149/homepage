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
import Control.Monad.Catch.OrphanInstances ()
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Data.Foldable
import qualified Servant

type ApplicationStack = BlogT |. ConfiguredT |. LoggingT' |. ConfigurableT |. IdentityT

newtype ApplicationT m a = ApplicationT { unApplicationT :: ApplicationStack m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadBlog)

deriving via Elevator ApplicationStack m
  instance
    ( MonadError Servant.ServerError m
    ) => MonadError Servant.ServerError (ApplicationT m)

runApplication :: (MonadIO m, MonadBaseControl IO m)
               => ApplicationT m a
               -> m a
runApplication app = do
  (preConfig, preConfigLog) <- runWriterLoggingT acquirePreConfig

  let
    runConfigurableT' :: ConfigurableT n a -> n a
    runConfigurableT' tma = runConfigurableT tma preConfig

    runLoggingT'' :: (MonadBaseControl IO n, MonadConfigurable n, MonadIO n) => LoggingT' n a -> n a
    runLoggingT'' tma = do
      maybeLogFile <- preConfigLogFile <$> preConfiguration
      runLoggingT' maybeLogFile $ do
        traverse_ logLine preConfigLog
        tma

    runConfiguredT' :: (MonadConfigurable n, MonadIO n, MonadLogger n) => ConfiguredT n a -> n a
    runConfiguredT' tma = do
      maybeConfig <- acquireConfig =<< preConfiguration
      case maybeConfig of
        Nothing -> error "No configuration."
        Just config -> runConfiguredT tma config

  runBlogT |. runConfiguredT' |. runLoggingT'' |. runConfigurableT' |. runIdentityT $ unApplicationT app
