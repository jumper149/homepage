{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Empty where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity

type EmptyT = Elevator NoT

runEmptyT :: EmptyT m a -> m a
runEmptyT = runIdentityT . runNoT . descend

newtype NoT m a = MkNoT {runNoT :: IdentityT m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
