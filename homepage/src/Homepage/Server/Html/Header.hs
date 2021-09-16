module Homepage.Server.Html.Header where

import Homepage.Server.Tab
import Homepage.Server.Html.Depth

import Data.Foldable
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

headerTab :: Natural -- ^ depth
          -> Tab
          -> Bool -- ^ active
          -> Html
headerTab depth tab active = a ! hrefWithDepth depth (toValue tabPath) ! classActive $ toMarkup tabName
  where TabDescription { tabName, tabPath } = describeTab tab
        classActive = if active
                         then class_ "active"
                         else mempty

headerTabsHelper :: Natural -- ^ depth
                 -> Maybe Tab
                 -> Html
headerTabsHelper depth activeTab = traverse_ f [ minBound .. maxBound ]
  where f :: Tab -> Html
        f tab = let active = case activeTab of
                               Just activeTab' -> tab == activeTab'
                               _ -> False
                 in headerTab depth tab active

headerTabs :: Natural -- ^ depth
           -> Maybe Tab
           -> Html
headerTabs depth activeTab =
  header $ H.div ! class_ "bar" $ do
    headerTabsHelper depth activeTab
    H.span $ a ! href "https://felixspringer.xyz/" $ "felixspringer.xyz"
