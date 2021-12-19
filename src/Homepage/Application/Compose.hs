{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Homepage.Application.Compose where

import Homepage.Application.Blog
import Homepage.Application.Configurable
import Homepage.Application.Configured
import Homepage.Application.Logging

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Writer.Class
import Data.Kind

newtype (|.)
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = ComposeT' { unComposeT' :: ComposeT t1 t2 m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadError e, MonadReader r, MonadState s, MonadWriter w)

deriving newtype
  instance
    ( forall m. Monad m => Monad (t2 m)
    , MonadTrans t1
    , MonadTrans t2
    ) => MonadTrans (t1 |. t2)

deriving newtype
  instance
    ( forall m. Monad m => Monad (t2 m)
    , MonadTransControl t1
    , MonadTransControl t2
    ) => MonadTransControl (t1 |. t2)

deriving newtype
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadIO m
    ) => MonadIO ((t1 |. t2) m)

deriving newtype
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadBase b m
    ) => MonadBase b ((t1 |. t2) m)

deriving newtype
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl (ComposeT t1 t2)
    , MonadBaseControl b m
    ) => MonadBaseControl b ((t1 |. t2) m)

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadBlog (t2 m)
    ) => MonadBlog ((t1 |. t2) m)

deriving via BlogT (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( MonadConfigured (t2 m)
    , MonadIO (t2 m)
    ) => MonadBlog ((BlogT |. t2) m)

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadConfigurable (t2 m)
    ) => MonadConfigurable ((t1 |. t2) m)

deriving via ConfigurableT (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( Monad (t2 m)
    ) => MonadConfigurable ((ConfigurableT |. t2) m)

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadConfigured (t2 m)
    ) => MonadConfigured ((t1 |. t2) m)

deriving via ConfiguredT (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( Monad (t2 m)
    ) => MonadConfigured ((ConfiguredT |. t2) m)

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadLogger (t2 m)
    ) => MonadLogger ((t1 |. t2) m)

deriving via LoggingT' (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( MonadIO (t2 m)
    ) => MonadLogger ((LoggingT' |. t2) m)

(|.) :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
     -> (forall a. t2 m a -> m (StT t2 a))
     -> (forall a. (t1 |. t2) m a -> m (StT t2 (StT t1 a)))
(|.) runT1 runT2 = runComposeT runT1 runT2 . unComposeT'

infixr 1 |.

(|.|) :: (t1 (t2 m) a -> t2 m a)
      -> (t2 m a -> m a)
      -> ((t1 |. t2) m a -> m a)
(|.|) runT1 runT2 = runComposeT' runT1 runT2 . unComposeT'

infixr 1 |.|
