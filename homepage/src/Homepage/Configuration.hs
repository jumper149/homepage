{-# LANGUAGE UndecidableInstances #-}

module Homepage.Configuration where

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Data.Word
import GHC.Generics

class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

data Configuration = Configuration
    { configDirectoryBlog :: FilePath
    , configDirectoryFiles :: FilePath
    , configDirectoryStatic :: FilePath
    , configPort :: Word16
    , configBaseUrl :: T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

newtype ConfiguredT m a = ConfiguredT { unConfiguredT :: ReaderT Configuration m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadTransControl, MonadBase b, MonadBaseControl b)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT
