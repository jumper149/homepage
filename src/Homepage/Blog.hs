module Homepage.Blog where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Calendar
import GHC.Generics

data BlogEntry = BlogEntry
    { blogContent :: T.Text
    , blogTitle :: T.Text
    , blogTimestamp :: Day
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

newtype BlogEntries = BlogEntries { unBlogEntries :: M.Map T.Text BlogEntry }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

lookupBlog :: T.Text -> BlogEntries -> Maybe BlogEntry
lookupBlog k = M.lookup k . unBlogEntries
