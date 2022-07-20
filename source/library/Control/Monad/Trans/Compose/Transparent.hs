{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Transparent where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity

type TransparentT = Elevator NoT

runTransparentT :: TransparentT m a -> m a
runTransparentT = runIdentityT . runNoT . descend

newtype NoT m a = MkNoT {runNoT :: IdentityT m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
