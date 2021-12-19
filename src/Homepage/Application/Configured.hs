{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance ( Monad (t m)
         , MonadTrans t
         , MonadConfigured m
         ) => MonadConfigured (Elevator t m) where
  configuration = Ascend $ lift configuration

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
