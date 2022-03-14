module Homepage.Application.Blog.Class where

import Homepage.Configuration.Blog

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import qualified Data.Text as T

class Monad m => MonadBlog m where
  blogEntries :: m BlogEntries
  readBlogEntryHtml :: BlogId -> m T.Text

instance ( Monad (t m)
         , MonadTrans t
         , MonadBlog m
         ) => MonadBlog (Elevator t m) where
  blogEntries = lift blogEntries
  readBlogEntryHtml = lift . readBlogEntryHtml

deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadBlog (t2 m)
    ) => MonadBlog (ComposeT t1 t2 m)
