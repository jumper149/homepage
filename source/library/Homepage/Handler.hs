{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Handler where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Handler.RequestHash

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.IO.Unlift.OrphanInstances ()
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose.Empty
import Control.Monad.Trans.Compose.Infix
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity

type StackT =
  EmptyT
    .| RequestHashT

newtype HandlerT m a = HandlerT {unHandlerT :: StackT m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadIO, MonadUnliftIO)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadBlog)
  deriving newtype (MonadError e)

runHandlerT :: MonadLogger m => Hash -> HandlerT m a -> m a
runHandlerT randomHash = runStackT . unHandlerT
 where
  runStackT =
    runEmptyT
      .| runRequestHashT randomHash . (logInfo "Starting HTTP request handler." >>)
