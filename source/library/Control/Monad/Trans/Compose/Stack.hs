{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Stack where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Kind

data Stack
  = NilT
  | Stack :||> ((Type -> Type) -> Type -> Type)

infixl 1 :||>

type family StackT (ts :: Stack) = (t :: (Type -> Type) -> Type -> Type) | t -> ts where
  StackT 'NilT = TransparentT
  StackT (ts ':||> t) = ComposeT t (StackT ts)

data RunStackT :: Stack -> (Type -> Type) -> Type -> Type where
  RunNilT :: RunStackT 'NilT m a
  (:..>) :: RunStackT ts m a -> (t (StackT ts m) a -> StackT ts m a) -> RunStackT (ts ':||> t) m a

infixl 1 :..>

runStackT :: RunStackT ts m a -> StackT ts m a -> m a
runStackT RunNilT = runTransparentT
runStackT (runRemainingStackT :..> runNextT) = runStackT runRemainingStackT . runNextT . deComposeT
