module Control.Monad.Trans.Compose.Infix where

import Control.Monad.Trans.Compose

type (.|) t2 t1 = ComposeT t1 t2

(.|) ::
  (t2 m a -> m a) ->
  (t1 (t2 m) a -> t2 m a) ->
  ((t2 .| t1) m a -> m a)
(.|) = flip runComposeT'

infixl 1 .|
