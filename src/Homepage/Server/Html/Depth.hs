module Homepage.Server.Html.Depth where

import qualified Data.Text as T
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

hrefWithDepth :: T.Text -- ^ base URL
              -> Maybe Natural -- ^ depth
              -> AttributeValue
              -> Attribute
hrefWithDepth baseUrl depth ref = href $ withDepth baseUrl depth ref

withDepth :: T.Text -- ^ base URL
          -> Maybe Natural -- ^ depth
          -> AttributeValue
          -> AttributeValue
withDepth baseUrl Nothing ref = textValue baseUrl <> "/" <> ref
withDepth _ (Just 0) ref = "./" <> ref
withDepth _ (Just 1) ref = "../" <> ref
withDepth baseUrl (Just n) ref = withDepth baseUrl (Just $ pred n) $  "../" <> ref
