module Homepage.Server.Html.Depth where

import qualified Data.Text as T
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

hrefWithDepth :: T.Text -- ^ base URL
              -> Maybe Natural -- ^ depth
              -> AttributeValue
              -> Attribute
hrefWithDepth baseUrl Nothing ref = href $ textValue baseUrl <> "/" <> ref
hrefWithDepth _ (Just 0) ref = href $ "./" <> ref
hrefWithDepth _ (Just 1) ref = href $ "../" <> ref
hrefWithDepth baseUrl (Just n) ref = hrefWithDepth baseUrl (Just $ pred n) $  "../" <> ref
