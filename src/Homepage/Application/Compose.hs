{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, TupleSections #-}

module Homepage.Application.Compose where

import Homepage.Application.Configurable
import Homepage.Application.Configured
import Homepage.Application.Logging

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Kind

newtype ComposeT
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = ComposeT { unComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT unComposeT $ \x -> f x
  restoreT = defaultRestoreT2 ComposeT

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadIO m) => MonadIO (ComposeT t1 t2 m) where
  liftIO = lift . liftIO

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadBase b m) => MonadBase b (ComposeT t1 t2 m) where
  liftBase = lift . liftBase

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadBaseControl b m) => MonadBaseControl b (ComposeT t1 t2 m) where
  type StM (ComposeT t1 t2 m) a = StM m (StT (ComposeT t1 t2) a)
  liftBaseWith f = liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT
  restoreM = restoreT . restoreM

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadThrow m) => MonadThrow (ComposeT t1 t2 m) where
  throwM = lift . throwM

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadCatch m) => MonadCatch (ComposeT t1 t2 m) where
  catch throwing catching = restoreT . pure =<< liftWith (\ runT -> catch (runT throwing) (runT . catching))

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadError e m) => MonadError e (ComposeT t1 t2 m) where
  throwError = lift . throwError
  catchError throwing catching = restoreT . pure =<< liftWith (\ runT -> catchError (runT throwing) (runT . catching))

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadReader r m) => MonadReader r (ComposeT t1 t2 m) where
  ask = lift ask
  local f tma = restoreT . pure =<< liftWith (\ runT -> Control.Monad.Reader.Class.local f $ runT tma)

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadState s m) => MonadState s (ComposeT t1 t2 m) where
  get = lift get
  put = lift . put

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadWriter w m) => MonadWriter w (ComposeT t1 t2 m) where
  tell = lift . tell
  listen tma = (\ (sta, w) -> (, w) <$> restoreT (pure sta)) =<< liftWith (\ runT -> listen $ runT tma)
  pass tma = lift . pass . pure =<< tma

instance {-# OVERLAPPABLE #-} (Monad (t1 (t2 m)), MonadTrans t1, MonadConfigurable (t2 m)) => MonadConfigurable (ComposeT t1 t2 m) where
  preConfiguration = ComposeT . lift $ preConfiguration

instance {-# OVERLAPPING #-} Monad (t2 m) => MonadConfigurable (ComposeT ConfigurableT t2 m) where
  preConfiguration = ComposeT preConfiguration

instance {-# OVERLAPPABLE #-} (Monad (t1 (t2 m)), MonadTrans t1, MonadConfigured (t2 m)) => MonadConfigured (ComposeT t1 t2 m) where
  configuration = ComposeT . lift $ configuration

instance {-# OVERLAPPING #-} Monad (t2 m) => MonadConfigured (ComposeT ConfiguredT t2 m) where
  configuration = ComposeT configuration

instance {-# OVERLAPPABLE #-} (Monad (t1 (t2 m)), MonadTrans t1, MonadLogger (t2 m)) => MonadLogger (ComposeT t1 t2 m) where
  monadLoggerLog loc logSource logLevel = ComposeT . lift . monadLoggerLog loc logSource logLevel

instance {-# OVERLAPPING #-} MonadIO (t2 m) => MonadLogger (ComposeT LoggingT' t2 m) where
  monadLoggerLog loc logSource logLevel = ComposeT . monadLoggerLog loc logSource logLevel

runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
            -> (forall a. t2 m a -> m (StT t2 a))
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . unComposeT

type (|.) = ComposeT

(|.) :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
     -> (forall a. t2 m a -> m (StT t2 a))
     -> (forall a. (t1 |. t2) m a -> m (StT t2 (StT t1 a)))
(|.) = runComposeT

infixr 1 |.

runComposeT' :: (t1 (t2 m) a -> t2 m a)
             -> (t2 m a -> m a)
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . unComposeT

(|.|) :: (t1 (t2 m) a -> t2 m a)
      -> (t2 m a -> m a)
      -> ((t1 |. t2) m a -> m a)
(|.|) = runComposeT'

infixr 1 |.|
