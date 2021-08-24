module Homepage.Server.Html.Header where

import Homepage.Server.Tab

import Data.Foldable
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

headerTab :: Tab
          -> Bool -- ^ active
          -> Html
headerTab tab active = a ! href (toValue tabPath) ! classActive $ toMarkup tabName
  where TabDescription { tabName, tabPath } = describeTab tab
        classActive = if active
                         then class_ "active"
                         else mempty

headerTabsHelper :: Maybe Tab
                 -> Html
headerTabsHelper activeTab = traverse_ f [ minBound .. maxBound ]
  where f :: Tab -> Html
        f tab = let active = case activeTab of
                               Just activeTab' -> tab == activeTab'
                               _ -> False
                 in headerTab tab active

headerTabs :: Maybe Tab
           -> Html
headerTabs activeTab =
  header $ H.div ! class_ "bar" $ do
    headerTabsHelper activeTab
    H.span $ a ! href "https://felixspringer.xyz/" $ "felixspringer.xyz"
