{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configurable where

import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

class Monad m => MonadConfigurable m where
  preConfiguration :: m PreConfiguration

instance ( Monad (t m)
         , MonadTrans t
         , MonadConfigurable m
         ) => MonadConfigurable (Elevator t m) where
  preConfiguration = Ascend $ lift preConfiguration

newtype ConfigurableT m a = ConfigurableT { unConfigurableT :: ReaderT PreConfiguration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadConfigurable (ConfigurableT m) where
  preConfiguration = ConfigurableT ask

runConfigurableT :: ConfigurableT m a -> PreConfiguration -> m a
runConfigurableT = runReaderT . unConfigurableT
