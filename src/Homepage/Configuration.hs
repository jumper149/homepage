module Homepage.Configuration where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Blog
import Homepage.Configuration.Contact
import Homepage.Configuration.Files

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Word
import qualified Deriving.Aeson as A
import GHC.Generics

data Configuration = Configuration
    { configRevision :: Maybe T.Text
    , configDirectoryBlog :: FilePath
    , configDirectoryFiles :: FilePath
    , configDirectoryStatic :: FilePath
    , configPort :: Word16
    , configBaseUrl :: BaseUrl
    , configContactInformation :: ContactInformation
    , configBlogEntries :: BlogEntries
    , configBlogPreviewMaxLength :: Maybe Word
    , configAtomPersonName :: T.Text
    , configAtomPersonEmail :: Maybe T.Text
    , configAtomMaxLength :: Maybe Word
    , configFileEntries :: FileEntries
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "config", A.CamelToKebab], A.RejectUnknownFields] Configuration

data PreConfiguration = PreConfiguration
    { preConfigConfigFile :: FilePath
    , preConfigLogFile :: Maybe FilePath
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
