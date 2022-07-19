module Control.Monad.Trans.Compose.Empty where

import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity

type EmptyT = Elevator IdentityT

runEmptyT :: EmptyT m a -> m a
runEmptyT = runIdentityT . descend
