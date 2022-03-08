module Homepage.Application.Environment.Class where

import Homepage.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator

class Monad m => MonadEnvironment m where
  environment :: m Environment

instance ( Monad (t m)
         , MonadTrans t
         , MonadEnvironment m
         ) => MonadEnvironment (Elevator t m) where
  environment = lift environment

deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadEnvironment (t2 m)
    ) => MonadEnvironment (ComposeT t1 t2 m)
