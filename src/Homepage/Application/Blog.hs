{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Compose
import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as T

class Monad m => MonadBlog m where
  blogEntries :: m BlogEntries
  readBlogEntryHtml :: BlogEntry -> m T.Text

instance ( Monad (t m)
         , MonadTrans t
         , MonadBlog m
         ) => MonadBlog (Elevator t m) where
  blogEntries = Ascend $ lift blogEntries
  readBlogEntryHtml = Ascend . lift . readBlogEntryHtml

deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadBlog (t2 m)
    ) => MonadBlog ((t1 |. t2) m)

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance (MonadConfigured m, MonadIO m) => MonadBlog (BlogT m) where
  blogEntries = BlogT $ lift $ configBlogEntries <$> configuration
  readBlogEntryHtml entry = do
    dir <- BlogT $ lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (blogContent entry) <> ".html"
    BlogT $ lift $ liftIO $ T.readFile file

deriving via BlogT (t2 (m :: * -> *))
  instance {-# OVERLAPPING #-}
    ( MonadConfigured (t2 m)
    , MonadIO (t2 m)
    ) => MonadBlog ((BlogT |. t2) m)

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT
