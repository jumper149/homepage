{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Application.Compose
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

instance (Monad (t1 (t2 m)), MonadTrans t1, MonadConfigured (t2 m)) => MonadConfigured ((t1 |. t2) m) where
  configuration = ComposeT' . ComposeT . lift $ configuration

instance {-# OVERLAPPING #-} Monad (t2 m) => MonadConfigured ((ConfiguredT |. t2) m) where
  configuration = ComposeT' $ ComposeT configuration

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
