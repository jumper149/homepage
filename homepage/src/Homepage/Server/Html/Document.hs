module Homepage.Server.Html.Document where

import Homepage.Server.Html.Depth
import Homepage.Server.Html.Header
import Homepage.Server.Tab

import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

document :: Natural -> Maybe Tab -> Html -> Html
document depth activeTab x =
  docTypeHtml ! lang "en" $ do
      H.head $ do
          meta ! charset "UTF-8"
          meta ! name "author" ! content "Felix Springer"
          meta ! name "description" ! content "I am some guy, live somewhere, like some stuff, spend some time, have some projects and you want to know about me."
          meta ! name "viewport" ! content "width=500"
          H.title $ "Felix Springer's " <> toMarkup titleName
          link ! rel "icon" ! hrefWithDepth depth "favicon.png"
          link ! rel "stylesheet" ! type_ "text/css" ! hrefWithDepth depth "stylesheet.css"
      body $ do
        headerTabs depth activeTab
        x
  where titleName = maybe "Homepage" (pageName . describeTab) activeTab
