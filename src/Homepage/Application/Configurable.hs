{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configurable where

import Homepage.Application.Compose
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
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

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadConfigurable (t2 m)
    ) => MonadConfigurable ((t1 |. t2) m)

newtype ConfigurableT m a = ConfigurableT { unConfigurableT :: ReaderT PreConfiguration m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadConfigurable (ConfigurableT m) where
  preConfiguration = ConfigurableT ask

deriving via ConfigurableT (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( Monad (t2 m)
    ) => MonadConfigurable ((ConfigurableT |. t2) m)

runConfigurableT :: ConfigurableT m a -> PreConfiguration -> m a
runConfigurableT = runReaderT . unConfigurableT
