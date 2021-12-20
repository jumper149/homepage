module Homepage.Blog where

import qualified Data.Aeson as A
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import Data.Time.Calendar
import GHC.Generics
import qualified Servant as S

newtype BlogId = BlogId { unBlogId :: T.Text }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSONKey, A.ToJSONKey)
  deriving newtype (S.FromHttpApiData, S.ToHttpApiData)

data BlogEntry = BlogEntry
    { blogTitle :: T.Text
    , blogTimestamp :: Day
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "blog", A.CamelToKebab], A.RejectUnknownFields] BlogEntry

newtype BlogEntries = BlogEntries { unBlogEntries :: M.Map BlogId BlogEntry }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.UnwrapUnaryRecords, A.RejectUnknownFields] BlogEntries

lookupBlog :: BlogId -> BlogEntries -> Maybe BlogEntry
lookupBlog k = M.lookup k . unBlogEntries

recentBlogEntries :: Maybe Word -> BlogEntries -> BlogEntries
recentBlogEntries count = BlogEntries . M.fromList . takeMaxLength . L.sortOn (Down . blogTimestamp . snd) . M.toList . unBlogEntries
  where
    takeMaxLength :: [a] -> [a]
    takeMaxLength = case count of
                      Nothing -> id
                      Just n -> take $ fromIntegral n
