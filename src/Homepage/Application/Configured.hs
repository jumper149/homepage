{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Configuration

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadTransControl, MonadBase b, MonadBaseControl b, MonadThrow, MonadCatch, MonadError e)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
