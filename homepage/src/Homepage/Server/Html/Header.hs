module Homepage.Server.Html.Header where

import Data.Foldable
import GHC.Generics
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

data HeaderTab =
      TabHome
    | TabBlog
    | TabProjects
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

headerTab :: HeaderTab
          -> Bool -- ^ active
          -> Html
headerTab tab active =
  case tab of
    TabHome -> addStyle "Home" ""
    TabBlog -> addStyle "Blog" "blog"
    TabProjects -> addStyle "Projects" "projects"
  where addStyle identifier path =
          if active
             then a ! class_ "active" ! href path $ identifier
             else a ! href path $ identifier

headerTabsHelper :: Maybe HeaderTab
                 -> Html
headerTabsHelper activeTab = traverse_ f [ minBound .. maxBound ]
    where f :: HeaderTab -> Html
          f tab = let active = case activeTab of
                                 Just activeTab' -> tab == activeTab'
                                 _ -> False
                   in headerTab tab active

headerTabs :: Maybe HeaderTab
           -> Html
headerTabs activeTab =
  header $ H.div ! class_ "bar" $ do
    headerTabsHelper activeTab
    H.span $ a ! href "https://felixspringer.xyz/" $ "felixspringer.xyz"
