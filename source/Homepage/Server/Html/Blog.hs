module Homepage.Server.Html.Blog where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Blog
import Homepage.Server.Html.Depth

import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord
import Data.Text qualified as T
import Data.Time.Calendar
import Numeric.Natural
import Text.Blaze.Html5

blogList :: BaseUrl
         -> Maybe Natural -- ^ depth
         -> BlogEntries
         -> Html
blogList baseUrl depth blogs = ul $
  toMarkup $ entryToMarkup <$> L.sortOn (Down . blogTimestamp . snd) (M.toList (unBlogEntries blogs))
  where
    entryToMarkup (blogId, BlogEntry { blogTitle, blogTimestamp }) =
      li $ do
        toMarkup $ T.pack (showGregorian blogTimestamp)
        " - "
        a ! hrefWithDepth baseUrl depth (textValue $ "blog/" <> unBlogId blogId) $ toMarkup blogTitle
