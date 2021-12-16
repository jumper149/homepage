{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Compose
import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration

import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import qualified Data.Text as T
import qualified Data.Text.IO as T

class Monad m => MonadBlog m where
  blogEntries :: m BlogEntries
  readBlogEntryHtml :: BlogEntry -> m T.Text

instance (MonadConfigured m, MonadIO m) => MonadBlog (BlogT m) where
  blogEntries = BlogT $ lift $ configBlogEntries <$> configuration
  readBlogEntryHtml entry = do
    dir <- BlogT $ lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (blogContent entry) <> ".html"
    BlogT $ lift $ liftIO $ T.readFile file

instance (Monad (t1 (t2 m)), MonadTrans t1, MonadBlog (t2 m)) => MonadBlog ((t1 |. t2) m) where
  blogEntries = ComposeT' . ComposeT . lift $ blogEntries
  readBlogEntryHtml = ComposeT' . ComposeT . lift . readBlogEntryHtml

instance {-# OVERLAPPING #-} (MonadConfigured (t2 m), MonadIO (t2 m)) => MonadBlog ((BlogT |. t2) m) where
  blogEntries = ComposeT' $ ComposeT blogEntries
  readBlogEntryHtml = ComposeT' . ComposeT . readBlogEntryHtml

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT
