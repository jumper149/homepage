{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT Control.Monad.Trans.Reader.ask

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
