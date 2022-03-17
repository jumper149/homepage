{-# LANGUAGE PolyKinds #-}

module Homepage.Application.Environment.Class where

import Homepage.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import GHC.TypeLits

class Monad m => MonadEnvironment m where
  environmentVariable :: KnownEnvVar name val envVar => EnvVar name -> m val

instance ( Monad (t m)
         , MonadTrans t
         , MonadEnvironment m
         ) => MonadEnvironment (Elevator t m) where
  environmentVariable = lift . environmentVariable

deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadEnvironment (t2 m)
    ) => MonadEnvironment (ComposeT t1 t2 m)

data EnvVar (name :: Symbol) = EnvVar
