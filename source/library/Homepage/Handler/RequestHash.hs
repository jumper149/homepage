{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Handler.RequestHash where

import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 qualified as B
import Data.Hashable
import Data.Kind
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed

newtype Hash = MkHash {getHash :: Word}

requestHash :: Request -> Hash
requestHash = MkHash . fromIntegral . hash . show

newtype RequestHashT m a = RequestHashT {unRequestHashT :: ReaderT Hash m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadLogger m => MonadLogger (RequestHashT m) where
  monadLoggerLog loc logSource logLevel logStr = do
    reqHash <- RequestHashT ask
    let reqInfo = "@[" <> show (getHash reqHash) <> "]"
    lift . monadLoggerLog loc logSource logLevel . toLogStr $
      B.pack reqInfo <> " " <> fromLogStr (toLogStr logStr)

deriving via
  RequestHashT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadLogger (t2 m) => MonadLogger (ComposeT RequestHashT t2 m)

runRequestHashT :: Hash -> RequestHashT m a -> m a
runRequestHashT reqHash = flip runReaderT reqHash . unRequestHashT

data RequestHash

instance HasServer api context => HasServer (RequestHash :> api) context where
  type ServerT (RequestHash :> api) m = Hash -> ServerT api m
  hoistServerWithContext Proxy pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route Proxy context subserver = route (Proxy @api) context $ passToServer subserver requestHash
