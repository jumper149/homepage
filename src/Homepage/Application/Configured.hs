{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Application.Configured.Class
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

deriving via ConfiguredT (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    ) => MonadConfigured (ComposeT ConfiguredT t2 m)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
