module Homepage.Blog where

import qualified Data.Aeson as A
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import Data.Time.Calendar
import GHC.Generics

data BlogEntry = BlogEntry
    { blogContent :: T.Text
    , blogTitle :: T.Text
    , blogTimestamp :: Day
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "blog", A.CamelToKebab], A.RejectUnknownFields] BlogEntry

newtype BlogEntries = BlogEntries { unBlogEntries :: M.Map T.Text BlogEntry }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.UnwrapUnaryRecords, A.RejectUnknownFields] BlogEntries

lookupBlog :: T.Text -> BlogEntries -> Maybe BlogEntry
lookupBlog k = M.lookup k . unBlogEntries

recentBlogEntries :: Word -> BlogEntries -> BlogEntries
recentBlogEntries n = BlogEntries . M.fromList . take (fromIntegral n) . L.sortOn (Down . blogTimestamp . snd) . M.toList . unBlogEntries
