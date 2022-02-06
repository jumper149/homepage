{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Environment where

import Homepage.Application.Environment.Class
import Homepage.Environment

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Proxy

newtype EnvironmentT m a = EnvironmentT { unEnvironmentT :: ReaderT Environment m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadEnvironment (EnvironmentT m) where
  environmentVariable :: forall name val (envVar :: EnvVarKind name val).
                         KnownEnvVar envVar
                      => EnvVar name
                      -> EnvironmentT m val
  environmentVariable _ = do
    environment <- EnvironmentT ask
    let accessEnvVar = getEnvironment environment
    pure $ getConst $ accessEnvVar $ caseEnvVar $ Proxy @name

deriving via EnvironmentT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance Monad (t2 m) => MonadEnvironment (ComposeT EnvironmentT t2 m)

runEnvironmentT :: Environment -> EnvironmentT m a -> m a
runEnvironmentT env tma = runReaderT (unEnvironmentT tma) env
