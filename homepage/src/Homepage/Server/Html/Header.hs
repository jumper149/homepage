{-# LANGUAGE NamedFieldPuns #-}

module Homepage.Server.Html.Header where

import Data.Foldable
import qualified Data.Text as T
import GHC.Generics
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

data HeaderTab =
      TabHome
    | TabBlog
    | TabProjects
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

data TabDescription = TabDescription
    { tabName :: T.Text
    , tabPath :: T.Text
    , pageName :: T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

describeTab :: HeaderTab -> TabDescription
describeTab TabHome = TabDescription
  { tabName = "Home"
  , tabPath = ""
  , pageName = "Homepage"
  }
describeTab TabBlog = TabDescription
  { tabName = "Blog"
  , tabPath = "blog"
  , pageName = "Blog"
  }
describeTab TabProjects = TabDescription
  { tabName = "Projects"
  , tabPath = "projects"
  , pageName = "Projects"
  }

headerTab :: HeaderTab
          -> Bool -- ^ active
          -> Html
headerTab tab active = a ! href (toValue tabPath) ! classActive $ toMarkup tabName
  where TabDescription { tabName, tabPath } = describeTab tab
        classActive = if active
                         then class_ "active"
                         else mempty

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
