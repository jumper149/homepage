{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application where

import Homepage.CLI
import Homepage.Application.Compose
import Homepage.Application.Configured

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Trans.Control

newtype ApplicationT m a = ApplicationT { unApplicationT :: (LoggingT |. ConfiguredT |. IdentityT) m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadThrow, MonadCatch)
  deriving newtype (MonadLogger)
  deriving newtype (MonadError e)
  deriving newtype (MonadConfigured)


runApplication :: MonadIO m
               => ApplicationT m a
               -> m a
runApplication app = do
  config <- liftIO launch
  let
    runConfiguredT' tma = runConfiguredT tma config
  runStdoutLoggingT |.| runConfiguredT' |.| runIdentityT $ unApplicationT app
