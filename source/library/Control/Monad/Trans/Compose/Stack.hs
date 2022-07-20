{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Stack where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Kind

type family StackT (ts :: [(Type -> Type) -> Type -> Type]) = (t :: (Type -> Type) -> Type -> Type) | t -> ts where
  StackT '[] = TransparentT
  StackT (t ': ts) = ComposeT t (StackT ts)

runStackT :: RunStackT ts m a -> StackT ts m a -> m a
runStackT RunTransparentT = runTransparentT
runStackT (RunNextT runRemainingStackT runNextT) = runStackT runRemainingStackT . runNextT . deComposeT

data RunStackT :: [(Type -> Type) -> Type -> Type] -> (Type -> Type) -> Type -> Type where
  RunTransparentT :: RunStackT '[] m a
  RunNextT :: RunStackT ts m a -> (t (StackT ts m) a -> StackT ts m a) -> RunStackT (t ': ts) m a
