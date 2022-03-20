module Homepage.Server.Html.Document where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Header
import Homepage.Server.Tab

import Data.Maybe
import Data.Text qualified as T
import Numeric.Natural
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

document :: BaseUrl
         -> ContactInformation
         -> Maybe T.Text -- ^ revision
         -> Maybe Natural -- ^ depth
         -> Maybe Tab -- ^ current tab
         -> Html -- ^ body
         -> Html
document baseUrl contactInformation maybeRev depth activeTab x =
  docTypeHtml ! lang "en" $ do
      H.head $ do
          meta ! charset "UTF-8"
          meta ! name "author" ! content (textValue $ contactName contactInformation)
          case maybeDescription of
            Just description -> meta ! name "description" ! content description
            _ -> pure ()
          meta ! name "viewport" ! content "width=500"
          H.title $ toMarkup (contactName contactInformation) <> "'s " <> toMarkup titleName
          link ! rel "icon" ! type_ "image/png" ! sizes "32x32" ! hrefWithDepth baseUrl depth "favicon.png"
          link ! rel "icon" ! type_ "image/png" ! sizes "192x192" ! hrefWithDepth baseUrl depth "favicon-192x192.png"
          link ! rel "icon" ! type_ "image/png" ! sizes "512x512" ! hrefWithDepth baseUrl depth "favicon-512x512.png"
          link ! rel "apple-touch-icon" ! type_ "image/png" ! sizes "512x512" ! hrefWithDepth baseUrl depth "favicon-512x512.png"
          link ! rel "stylesheet" ! type_ "text/css" ! hrefWithDepth baseUrl depth "stylesheet.css"
      body $ do
        headerTabs baseUrl contactInformation depth activeTab
        x
        H.div ! class_ "footer" $ do
          let withHref = case contactSourceUrl contactInformation of
                           Nothing -> Prelude.id
                           Just url -> (! href (textValue url))
          withHref a $ toMarkup $ fromMaybe "unknown-revision" maybeRev
  where
    titleName = maybe "Homepage" (tabPageName . describeTab) activeTab
    maybeDescription = textValue <$> (tabMetaDescription . describeTab =<< activeTab)
