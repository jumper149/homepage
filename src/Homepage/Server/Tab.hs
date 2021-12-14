module Homepage.Server.Tab where

import qualified Data.Text as T
import GHC.Generics

data Tab =
      TabHome
    | TabBlog
    | TabFiles
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

data TabDescription = TabDescription
    { tabName :: T.Text
    , tabPath :: T.Text
    , pageName :: T.Text
    , metaDescription :: Maybe T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

describeTab :: Tab -> TabDescription
describeTab TabHome = TabDescription
    { tabName = "Home"
    , tabPath = ""
    , pageName = "Homepage"
    , metaDescription = Just "I am some guy, live somewhere, like some stuff, spend some time, have some projects and you want to know about me."
    }
describeTab TabBlog = TabDescription
    { tabName = "Blog"
    , tabPath = "blog"
    , pageName = "Blog"
    , metaDescription = Nothing
    }
describeTab TabFiles = TabDescription
    { tabName = "Files"
    , tabPath = "files"
    , pageName = "Files"
    , metaDescription = Nothing
    }
