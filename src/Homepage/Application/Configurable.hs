{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configurable where

import Homepage.Application.Configurable.Class
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

newtype ConfigurableT m a = ConfigurableT { unConfigurableT :: ReaderT PreConfiguration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadConfigurable (ConfigurableT m) where
  preConfiguration = ConfigurableT ask

deriving via ConfigurableT (t2 (m :: * -> *))
  instance Monad (t2 m) => MonadConfigurable (ComposeT ConfigurableT t2 m)

runConfigurableT :: ConfigurableT m a -> PreConfiguration -> m a
runConfigurableT = runReaderT . unConfigurableT
