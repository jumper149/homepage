{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Homepage.Application.Compose where

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
    = ComposeT { unComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)
  deriving newtype (MonadBase b, MonadBaseControl b)
  deriving newtype (MonadThrow, MonadCatch)
  deriving newtype (MonadError e, MonadReader r, MonadState s, MonadWriter w)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT unComposeT $ \x -> f x
  restoreT = defaultRestoreT2 ComposeT

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
