module Homepage.Server.Html.Depth where

import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

hrefWithDepth :: Natural -> AttributeValue -> Attribute
hrefWithDepth 0 ref = href $ "./" <> ref
hrefWithDepth 1 ref = href $ "../" <> ref
hrefWithDepth n ref = hrefWithDepth (pred n) $  "../" <> ref
