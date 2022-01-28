{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Logger.OrphanInstances () where

import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator

instance (Monad (t m), MonadTrans t, MonadLogger m) => MonadLogger (Elevator t m) where
  monadLoggerLog loc logSource logLevel = Ascend . lift . monadLoggerLog loc logSource logLevel

deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadLogger (t2 m)
    ) => MonadLogger (ComposeT t1 t2 m)

deriving via LoggingT (t2 (m :: * -> *))
  instance
    ( MonadIO (t2 m)
    ) => MonadLogger (ComposeT LoggingT t2 m)
