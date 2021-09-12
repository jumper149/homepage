module Homepage.Blog where

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

newtype BlogEntries = BlogEntries { unBlogEntries :: M.Map T.Text BlogEntry }
  deriving stock (Eq, Generic, Ord, Read, Show)

lookupBlog :: T.Text -> BlogEntries -> Maybe BlogEntry
lookupBlog k = M.lookup k . unBlogEntries

blogEntries :: BlogEntries
blogEntries = BlogEntries $ M.fromList
  [ (,) "myOwnImplementationOfIExpressions" $ BlogEntry
      { blogContent = "myOwnImplementationOfIExpressions"
      , blogTitle = "my own Implementation of I-Expressions"
      , blogTimestamp = fromGregorian 2019 06 30
      }
  ]
