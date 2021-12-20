{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as T

class Monad m => MonadBlog m where
  blogEntries :: m BlogEntries
  readBlogEntryHtml :: BlogId -> m T.Text

instance ( Monad (t m)
         , MonadTrans t
         , MonadBlog m
         ) => MonadBlog (Elevator t m) where
  blogEntries = Ascend $ lift blogEntries
  readBlogEntryHtml = Ascend . lift . readBlogEntryHtml

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance (MonadConfigured m, MonadIO m) => MonadBlog (BlogT m) where
  blogEntries = BlogT $ lift $ configBlogEntries <$> configuration
  readBlogEntryHtml blogId = do
    dir <- BlogT $ lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (unBlogId blogId) <> ".html"
    BlogT $ lift $ liftIO $ T.readFile file

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT
