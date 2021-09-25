module Homepage.Configuration where

import Homepage.Blog

import qualified Data.Text as T
import Data.Word
import GHC.Generics

data Configuration = Configuration
    { configDirectoryBlog :: FilePath
    , configDirectoryFiles :: FilePath
    , configDirectoryStatic :: FilePath
    , configBlogEntries :: BlogEntries
    , configPort :: Word16
    , configBaseUrl :: T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
