{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Configuration

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT Control.Monad.Trans.Reader.ask

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadThrow, MonadCatch)
  deriving newtype (MonadError e, MonadState s, MonadWriter w)

instance MonadReader r m => MonadReader r (ConfiguredT m) where
  ask = lift Control.Monad.Reader.Class.ask
  local f tma = liftWith $ \ runT -> Control.Monad.Reader.Class.local f $ runT tma

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
