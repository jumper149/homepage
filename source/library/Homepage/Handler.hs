{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Handler where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Handler.RequestHash
import Homepage.Server.Route

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Infix
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity
import Servant
import Servant.API.Generic

type API = ToServantApi Routes
type WrappedAPI = RequestHash :> API

hoistServerRunHandlerT :: MonadLogger m => ServerT API (HandlerT m) -> ServerT WrappedAPI m
hoistServerRunHandlerT server randomHash = hoistServer (Proxy @API) (runHandlerT randomHash) server

type StackT =
  Elevator IdentityT
    .| RequestHashT

newtype HandlerT m a = HandlerT {unHandlerT :: StackT m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadBlog)
  deriving newtype (MonadError e)

runHandlerT :: MonadLogger m => Hash -> HandlerT m a -> m a
runHandlerT randomHash handler =
  runIdentityT . descend
    .| runRequestHashT randomHash . (logInfo "Starting HTTP request handler." >>)
    $ unHandlerT handler
