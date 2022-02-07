{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application where

import Homepage.Application.Blog
import Homepage.Application.Blog.Class
import Homepage.Application.Configurable
import Homepage.Application.Configurable.Class
import Homepage.Application.Configured
import Homepage.Application.Configured.Class
import Homepage.Application.Logging
import Homepage.Configuration
import Homepage.Configuration.Acquisition

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Data.Foldable
import qualified Servant

type (|.) = ComposeT

(|.) :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
     -> (forall a. t2 m a -> m (StT t2 a))
     -> (forall a. (t1 |. t2) m a -> m (StT t2 (StT t1 a)))
(|.) = runComposeT

infixr 1 |.

type StackT = BlogT |. ConfiguredT |. LoggingT' |. ConfigurableT |. IdentityT

newtype ApplicationT m a = ApplicationT { unApplicationT :: StackT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)

deriving newtype instance (MonadBaseControl IO m, MonadIO m) => MonadBlog (ApplicationT m)

deriving via Elevator ApplicationT m
  instance
    ( MonadError Servant.ServerError m
    ) => MonadError Servant.ServerError (ApplicationT m)

runApplication :: (MonadIO m, MonadBaseControl IO m)
               => ApplicationT m a
               -> m a
runApplication app = do
  (preConfig, preConfigLog) <- runWriterLoggingT acquirePreConfig

  runCheckedBlogT |.
    runConfiguredT' |.
      runLoggingT'' preConfigLog |.
        runConfigurableT' preConfig |.
          runIdentityT $ unApplicationT app

  where
    runConfigurableT' :: PreConfiguration -> ConfigurableT n a -> n a
    runConfigurableT' preConfig tma = runConfigurableT tma preConfig

    runLoggingT'' :: (MonadBaseControl IO n, MonadConfigurable n, MonadIO n) => [LogLine] -> LoggingT' n a -> n a
    runLoggingT'' preLog tma = do
      maybeLogFile <- preConfigLogFile <$> preConfiguration
      runLoggingT' maybeLogFile $ do
        traverse_ logLine preLog
        tma

    runConfiguredT' :: (MonadConfigurable n, MonadIO n, MonadLogger n) => ConfiguredT n a -> n a
    runConfiguredT' tma = do
      maybeConfig <- acquireConfig =<< preConfiguration
      case maybeConfig of
        Nothing -> error "No configuration."
        Just config -> runConfiguredT tma config
