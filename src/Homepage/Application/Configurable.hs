{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configurable where

import Homepage.Application.Compose
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

class Monad m => MonadConfigurable m where
  preConfiguration :: m PreConfiguration

instance Monad m => MonadConfigurable (ConfigurableT m) where
  preConfiguration = ConfigurableT Control.Monad.Trans.Reader.ask

instance {-# OVERLAPPABLE #-} (Monad (t1 (t2 m)), MonadTrans t1, MonadConfigurable (t2 m)) => MonadConfigurable (ComposeT t1 t2 m) where
  preConfiguration = ComposeT . lift $ preConfiguration

instance {-# OVERLAPPING #-} Monad (t2 m) => MonadConfigurable (ComposeT ConfigurableT t2 m) where
  preConfiguration = ComposeT preConfiguration

newtype ConfigurableT m a = ConfigurableT { unConfigurableT :: ReaderT PreConfiguration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runConfigurableT :: ConfigurableT m a -> PreConfiguration -> m a
runConfigurableT = runReaderT . unConfigurableT
