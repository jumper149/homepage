module Homepage.Server.Tab where

import qualified Data.Text as T
import GHC.Generics

data Tab =
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

describeTab :: Tab -> TabDescription
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
