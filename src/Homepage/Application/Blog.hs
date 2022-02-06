{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog

import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Foldable
import qualified Data.Map as M
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

runCheckedBlogT :: (MonadConfigured m, MonadError IOException m, MonadIO m, MonadLogger m)
                => BlogT m a
                -> m a
runCheckedBlogT tma = runBlogT $ do
  entries <- M.toList . unBlogEntries <$> blogEntries
  let checkBlogEntry blogId blogEntry = do
        lift $ $logInfo $ "Check blog entry '" <> T.pack (show (blogId, blogEntry)) <> "'."
        liftWith $ \ runT ->
          catchError (void $ runT $ readBlogEntryHtml blogId) $ \ err ->
            $logWarn $ "Failed to read HTML for blog entry '" <> T.pack (show blogId) <> "' with '" <> T.pack (show err) <> "'."
  traverse_ (uncurry checkBlogEntry) entries
  tma
