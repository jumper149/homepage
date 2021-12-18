{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, TupleSections #-}

module Control.Monad.Trans.Compose where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class
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
    = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \x -> f x
  restoreT = defaultRestoreT2 ComposeT

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadIO m) => MonadIO (ComposeT t1 t2 m) where
  liftIO = lift . liftIO

instance (Monad (t1 (t2 m)), MonadTrans (ComposeT t1 t2), MonadBase b m) => MonadBase b (ComposeT t1 t2 m) where
  liftBase = lift . liftBase

instance (Monad (t1 (t2 m)), MonadTransControl (ComposeT t1 t2), MonadBaseControl b m) => MonadBaseControl b (ComposeT t1 t2 m) where
  type StM (ComposeT t1 t2 m) a = StM m (StT (ComposeT t1 t2) a)
  liftBaseWith f = liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT
  restoreM = restoreT . restoreM

instance (Monad (t1 (t2 m)), MonadTrans t1, MonadThrow (t2 m)) => MonadThrow (ComposeT t1 t2 m) where
  throwM = ComposeT . lift . throwM

instance (Monad (t1 (t2 m)), MonadTransControl t1, MonadCatch (t2 m)) => MonadCatch (ComposeT t1 t2 m) where
  catch throwing catching = ComposeT $ restoreT . pure =<< liftWith (\ runT -> catch (runT $ deComposeT throwing) (runT . deComposeT . catching))

instance (Monad (t1 (t2 m)), MonadTransControl t1, MonadError e (t2 m)) => MonadError e (ComposeT t1 t2 m) where
  throwError = ComposeT . lift . throwError
  catchError throwing catching = ComposeT $ restoreT . pure =<< liftWith (\ runT -> catchError (runT $ deComposeT throwing) (runT . deComposeT . catching))

instance (Monad (t1 (t2 m)), MonadTransControl t1, MonadReader r (t2 m)) => MonadReader r (ComposeT t1 t2 m) where
  ask = ComposeT $ lift ask
  local f tma = ComposeT $ restoreT . pure =<< liftWith (\ runT -> Control.Monad.Reader.Class.local f $ runT $ deComposeT tma)

instance (Monad (t1 (t2 m)), MonadTrans t1, MonadState s (t2 m)) => MonadState s (ComposeT t1 t2 m) where
  get = ComposeT $ lift get
  put = ComposeT . lift . put

instance (Monad (t1 (t2 m)), MonadTransControl t1, MonadWriter w (t2 m)) => MonadWriter w (ComposeT t1 t2 m) where
  tell = ComposeT . lift . tell
  listen tma = ComposeT $ (\ (sta, w) -> (, w) <$> restoreT (pure sta)) =<< liftWith (\ runT -> listen $ runT $ deComposeT tma)
  pass tma = ComposeT $ lift . pass . pure =<< deComposeT tma

runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
            -> (forall a. t2 m a -> m (StT t2 a))
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

runComposeT' :: (t1 (t2 m) a -> t2 m a)
             -> (t2 m a -> m a)
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT
