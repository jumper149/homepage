module Homepage.Files where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import Data.Time.Calendar
import GHC.Generics

data FileFormat = FileFormat
    { fileFormatName :: T.Text
    , fileFormatExtension :: Maybe T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "fileFormat", A.CamelToKebab], A.RejectUnknownFields] FileFormat

data FileEntry = FileEntry
    { fileIdentifier :: T.Text
    , fileFormats :: [FileFormat]
    , fileName :: T.Text
    , fileSection :: Maybe T.Text
    , fileTimestamp :: Day
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "file", A.CamelToKebab], A.RejectUnknownFields] FileEntry

newtype FileEntries = FileEntries { unFileEntries :: S.Set FileEntry }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.UnwrapUnaryRecords, A.RejectUnknownFields] FileEntries

groupFiles :: S.Set FileEntry -> M.Map (Maybe T.Text) (S.Set FileEntry)
groupFiles entries = M.fromList $ sectionGroup <$> S.toList sections
  where
    sections = S.map fileSection entries
    sectionGroup section = (section, S.filter ((section ==). fileSection) entries)
