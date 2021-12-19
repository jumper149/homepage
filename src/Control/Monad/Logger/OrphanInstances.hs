{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Logger.OrphanInstances () where

import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Elevator

instance (Monad (t m), MonadTrans t, MonadLogger m) => MonadLogger (Elevator t m) where
  monadLoggerLog loc logSource logLevel = Ascend . lift . monadLoggerLog loc logSource logLevel
