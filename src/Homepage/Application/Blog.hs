{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance (MonadConfigured m, MonadIO m) => MonadBlog (BlogT m) where
  blogEntries = BlogT $ lift $ configBlogEntries <$> configuration
  readBlogEntryHtml blogId = do
    dir <- BlogT $ lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (unBlogId blogId) <> ".html"
    BlogT $ lift $ liftIO $ T.readFile file

deriving via BlogT (t2 (m :: * -> *))
  instance (MonadConfigured (t2 m), MonadIO (t2 m)) => MonadBlog (ComposeT BlogT t2 m)

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT
