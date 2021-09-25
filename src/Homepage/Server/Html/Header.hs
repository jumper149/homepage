module Homepage.Server.Html.Header where

import Homepage.Server.Tab
import Homepage.Server.Html.Depth

import Data.Foldable
import qualified Data.Text as T
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

headerTab :: T.Text -- ^ base URL
          -> Maybe Natural -- ^ depth
          -> Tab
          -> Bool -- ^ active
          -> Html
headerTab baseUrl depth tab active = a ! hrefWithDepth baseUrl depth (toValue tabPath) ! classActive $ toMarkup tabName
  where TabDescription { tabName, tabPath } = describeTab tab
        classActive = if active
                         then class_ "active"
                         else mempty

headerTabsHelper :: T.Text -- ^ base URL
                 -> Maybe Natural -- ^ depth
                 -> Maybe Tab
                 -> Html
headerTabsHelper baseUrl depth activeTab = traverse_ f [ minBound .. maxBound ]
  where f :: Tab -> Html
        f tab = let active = case activeTab of
                               Just activeTab' -> tab == activeTab'
                               _ -> False
                 in headerTab baseUrl depth tab active

headerTabs :: T.Text -- ^ base URL
           -> Maybe Natural -- ^ depth
           -> Maybe Tab
           -> Html
headerTabs baseUrl depth activeTab =
  header $ H.div ! class_ "bar" $ do
    headerTabsHelper baseUrl depth activeTab
    H.span $ a ! href (textValue baseUrl) $ "felixspringer.xyz" -- TODO: base URL naming is hardcoded
