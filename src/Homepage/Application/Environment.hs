{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Environment where

import Homepage.Application.Environment.Class
import Homepage.Environment

import Control.Applicative (Const (..))
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

newtype EnvironmentT m a = EnvironmentT { unEnvironmentT :: ReaderT Environment m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadEnvironment (EnvironmentT m) where
  environmentVariable proxy = getConst . askEnvironmentVariable proxy <$> EnvironmentT ask

deriving via EnvironmentT (t2 (m :: * -> *))
  instance Monad (t2 m) => MonadEnvironment (ComposeT EnvironmentT t2 m)

runEnvironmentT :: EnvironmentT m a -> Environment -> m a
runEnvironmentT = runReaderT . unEnvironmentT
