module Homepage.Server.Html.Blog where

import Homepage.Blog
import Homepage.Server.Html.Depth

import Data.List
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time.Calendar
import Numeric.Natural
import Text.Blaze.Html5

blogList :: Natural -> BlogEntries -> Html
blogList depth blogs = ul $
  toMarkup $ entryToMarkup <$> sortOn (Down . blogTimestamp . snd) (M.toList (unBlogEntries blogs))
  where
    entryToMarkup (blogKey, BlogEntry { blogTitle, blogTimestamp }) =
      li $ p $ do
        toMarkup $ T.pack (showGregorian blogTimestamp)
        " - "
        a ! hrefWithDepth depth (textValue $ "blog/" <> blogKey) $ toMarkup blogTitle
