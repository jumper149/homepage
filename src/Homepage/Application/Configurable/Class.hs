module Homepage.Application.Configurable.Class where

import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator

class Monad m => MonadConfigurable m where
  preConfiguration :: m PreConfiguration

instance ( Monad (t m)
         , MonadTrans t
         , MonadConfigurable m
         ) => MonadConfigurable (Elevator t m) where
  preConfiguration = lift preConfiguration

deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadConfigurable (t2 m)
    ) => MonadConfigurable (ComposeT t1 t2 m)
