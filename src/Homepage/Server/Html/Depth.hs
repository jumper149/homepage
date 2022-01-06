module Homepage.Server.Html.Depth where

import Homepage.BaseUrl

import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

hrefWithDepth :: BaseUrl
              -> Maybe Natural -- ^ depth
              -> AttributeValue
              -> Attribute
hrefWithDepth baseUrl depth ref = href $ withDepth baseUrl depth ref

withDepth :: BaseUrl
          -> Maybe Natural -- ^ depth
          -> AttributeValue
          -> AttributeValue
withDepth baseUrl Nothing ref = textValue (displayBaseUrl baseUrl) <> ref
withDepth _ (Just 0) ref = "./" <> ref
withDepth _ (Just 1) ref = "../" <> ref
withDepth baseUrl (Just n) ref = withDepth baseUrl (Just $ pred n) $  "../" <> ref
