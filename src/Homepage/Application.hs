{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application where

import Homepage.Application.Blog
import Homepage.Application.Blog.Class
import Homepage.Application.Configured
import Homepage.Application.Configured.Class
import Homepage.Application.Environment
import Homepage.Application.Environment.Acquisition
import Homepage.Application.Logging

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Data.Foldable
import Servant qualified

type (.|) t2 t1 = ComposeT t1 t2

(.|) :: (forall a. t2 m a -> m (StT t2 a))
     -> (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
     -> (forall a. (t2 .| t1) m a -> m (StT t2 (StT t1 a)))
(.|) runT2 runT1 = runComposeT runT1 runT2

infixl 1 .|

type StackT = IdentityT
           .| EnvironmentT
           .| LoggingT'
           .| ConfiguredT
           .| BlogT

newtype ApplicationT m a = ApplicationT { unApplicationT :: StackT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)

deriving newtype instance (MonadBaseControl IO m, MonadIO m) => MonadBlog (ApplicationT m)

deriving via Elevator ApplicationT m
  instance MonadError Servant.ServerError m => MonadError Servant.ServerError (ApplicationT m)

runApplicationT :: (MonadIO m, MonadBaseControlIdentity IO m)
                => ApplicationT m a
                -> m a
runApplicationT app = do
  (env, preLog) <- runWriterLoggingT $ do
    logInfo "Startup."
    acquireEnvironment

  let runStackT = runIdentityT
               .| runEnvironmentT env
               .| runAppLoggingT' . (traverse_ logLine preLog >>)
               .| runAppConfiguredT
               .| runAppBlogT

  runStackT $ unApplicationT app
