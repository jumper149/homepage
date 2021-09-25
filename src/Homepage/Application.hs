{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application where

import Homepage.CLI
import Homepage.Application.Compose
import Homepage.Application.Configured

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

newtype ApplicationT m a = ApplicationT { unApplicationT :: (ConfiguredT |. LoggingT) m a }
  deriving newtype (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl, MonadThrow, MonadCatch, MonadError e)

instance Monad m => MonadConfigured (ApplicationT m) where
  configuration = ApplicationT $ ComposeT configuration

runApplication :: (MonadBase IO m, MonadIO m)
               => ApplicationT m a
               -> m a
runApplication app = do
  config <- liftBase launch
  let
    runConfiguredT' tma = runConfiguredT tma config
  runConfiguredT' |.| runStdoutLoggingT $ unApplicationT app