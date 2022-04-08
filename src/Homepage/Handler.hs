{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Handler where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Handler.RequestHash
import Homepage.Server.Route

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity
import Servant
import Servant.API.Generic

type API = ToServantApi Routes
type WrappedAPI = RequestHash :> API

type (.|) t2 t1 = ComposeT t1 t2

(.|) :: (t2 m a -> m a)
     -> (t1 (t2 m) a -> t2 m a)
     -> ((t2 .| t1) m a -> m a)
(.|) = flip runComposeT'

infixl 1 .|

type StackT = Elevator IdentityT
           .| RequestHashT

newtype HandlerT m a = HandlerT { unHandlerT :: StackT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype MonadLogger
  deriving newtype MonadConfigured
  deriving newtype MonadBlog
  deriving newtype (MonadError e)

runHandlerT :: Hash -> HandlerT m a -> m a
runHandlerT randomHash = (runIdentityT . descend .| runRequestHashT randomHash) . unHandlerT

wrapApi :: ServerT API (HandlerT m) -> ServerT WrappedAPI m
wrapApi server randomHash = hoistServer (Proxy @API) (runHandlerT randomHash) server
