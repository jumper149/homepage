module Homepage.Server.Html.Document where

import Homepage.Server.Html.Depth
import Homepage.Server.Html.Header
import Homepage.Server.Tab

import qualified Data.Text as T
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

document :: T.Text -- ^ base URL
         -> Maybe Natural -- ^ depth
         -> Maybe Tab -- ^ current tab
         -> Html -- ^ body
         -> Html
document baseUrl depth activeTab x =
  docTypeHtml ! lang "en" $ do
      H.head $ do
          meta ! charset "UTF-8"
          meta ! name "author" ! content "Felix Springer"
          meta ! name "description" ! content "I am some guy, live somewhere, like some stuff, spend some time, have some projects and you want to know about me."
          meta ! name "viewport" ! content "width=500"
          H.title $ "Felix Springer's " <> toMarkup titleName
          link ! rel "icon" ! hrefWithDepth baseUrl depth "favicon.png"
          link ! rel "stylesheet" ! type_ "text/css" ! hrefWithDepth baseUrl depth "stylesheet.css"
      body $ do
        headerTabs baseUrl depth activeTab
        x
  where titleName = maybe "Homepage" (pageName . describeTab) activeTab
