{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Environment where

import Homepage.Application.Environment.Class
import Homepage.Environment

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Proxy

newtype Environment = MkEnvironment { getEnvironment :: forall name val. EnvVarKind name val -> Const val name }

newtype EnvironmentT m a = EnvironmentT { unEnvironmentT :: ReaderT Environment m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance Monad m => MonadEnvironment (EnvironmentT m) where
  environmentVariable named = lookupEnvVar (proxy named) <$> EnvironmentT ask
    where
      proxy :: EnvVar name -> Proxy name
      proxy _ = Proxy
      lookupEnvVar :: forall name val (envVar :: EnvVarKind name val). KnownEnvVar envVar => Proxy name -> Environment -> val
      lookupEnvVar p env = getConst $ getEnvironment env $ caseEnvVar p

deriving via EnvironmentT ((t2 :: (Type -> Type) -> Type -> Type) (m :: Type -> Type))
  instance Monad (t2 m) => MonadEnvironment (ComposeT EnvironmentT t2 m)

runEnvironmentT :: Environment -> EnvironmentT m a -> m a
runEnvironmentT env tma = runReaderT (unEnvironmentT tma) env
