{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Compose
import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration

import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics

data BlogFormat = BlogFormatHTML
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

class Monad m => MonadBlog m where
  blogEntries :: m BlogEntries
  readBlogEntry :: BlogFormat -> BlogEntry -> m T.Text

instance (MonadConfigured m, MonadIO m) => MonadBlog (BlogT m) where
  blogEntries = BlogT $ lift $ configBlogEntries <$> configuration
  readBlogEntry BlogFormatHTML entry = do
    dir <- BlogT $ lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (blogContent entry) <> ".html"
--    $logInfo $ "Read blog article from file: " <> T.pack (show file)
    BlogT $ lift $ liftIO $ T.readFile file

instance {-# OVERLAPPABLE #-} (Monad (t1 (t2 m)), MonadTrans t1, MonadBlog (t2 m)) => MonadBlog (ComposeT t1 t2 m) where
  blogEntries = ComposeT . lift $ blogEntries
  readBlogEntry format entry = ComposeT . lift $ readBlogEntry format entry

instance (MonadConfigured (t2 m), MonadIO (t2 m)) => MonadBlog (ComposeT BlogT t2 m) where
  blogEntries = ComposeT blogEntries
  readBlogEntry format entry = ComposeT $ readBlogEntry format entry

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT
