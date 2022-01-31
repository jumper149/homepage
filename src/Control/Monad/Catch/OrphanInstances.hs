{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Catch.OrphanInstances () where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator

instance (Monad (t m), MonadTrans t, MonadThrow m) => MonadThrow (Elevator t m) where
  throwM = lift . throwM

instance (Monad (t m), MonadTransControl t, MonadCatch m) => MonadCatch (Elevator t m) where
  catch throwing catching = restoreT . pure =<< liftWith (\ runT -> catch (runT throwing) (runT . catching))
