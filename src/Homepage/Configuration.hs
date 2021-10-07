module Homepage.Configuration where

import Homepage.Blog

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import Data.Word
import GHC.Generics

data Configuration = Configuration
    { configDirectoryBlog :: FilePath
    , configDirectoryFiles :: FilePath
    , configDirectoryStatic :: FilePath
    , configPort :: Word16
    , configBaseUrl :: T.Text
    , configBlogEntries :: BlogEntries
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "config", A.CamelToKebab], A.RejectUnknownFields] Configuration

data PreConfiguration = PreConfiguration
    { preConfigConfigFile :: FilePath
    , preConfigLogFile :: Maybe FilePath
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
