{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog

import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Foldable
import Data.Kind
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T

newtype BlogT m a = BlogT { unBlogT :: IdentityT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance (MonadBaseControl IO m, MonadConfigured m, MonadLogger m) => MonadBlog (BlogT m) where
  blogEntries = lift $ configBlogEntries <$> configuration
  readBlogEntryHtml blogId = do
    dir <- lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (unBlogId blogId) <> ".html"
    lift $ (restoreM =<<) $ liftBaseWith $ \ runInBase ->
      catchError (runInBase $ liftBase $ T.readFile file) $ \ err -> runInBase $ do
        $logWarn $ "Failed to read HTML for blog entry '" <> T.pack (show blogId) <> "' with '" <> T.pack (show err) <> "'."
        pure (undefined :: T.Text)

deriving via BlogT ((t2 :: (Type -> Type) -> Type -> Type) (m :: Type -> Type))
  instance (MonadBaseControl IO (t2 m), MonadConfigured (t2 m), MonadLogger (t2 m)) => MonadBlog (ComposeT BlogT t2 m)

runBlogT :: BlogT m a
         -> m a
runBlogT = runIdentityT . unBlogT

runAppBlogT :: (MonadBaseControl IO m, MonadConfigured m, MonadLogger m)
            => BlogT m a
            -> m a
runAppBlogT tma = runBlogT $ do
  entries <- M.toList . unBlogEntries <$> blogEntries
  let checkBlogEntry blogId blogEntry = do
        lift $ $logInfo $ "Check blog entry '" <> T.pack (show (blogId, blogEntry)) <> "'."
        void $ readBlogEntryHtml blogId
  traverse_ (uncurry checkBlogEntry) entries
  tma
