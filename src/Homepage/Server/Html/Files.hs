module Homepage.Server.Html.Files where

import Homepage.Files
import Homepage.Server.Html.Depth

import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Calendar
import Numeric.Natural
import Text.Blaze.Html5

fileList :: T.Text -- ^ base URL
         -> Maybe Natural -- ^ depth
         -> FileEntries
         -> Html
fileList baseUrl depth files = ul $
  toMarkup $ fileToMarkup <$> sortOn (Down . fileTimestamp) (S.toList (unFileEntries files))
  where
    fileToMarkup file@FileEntry { fileName, fileTimestamp } =
      li $ do
        toMarkup $ T.pack (showGregorian fileTimestamp)
        " - "
        toMarkup fileName
        " "
        fileFormatList baseUrl depth file

fileFormatList :: T.Text -- ^ base URL
               -> Maybe Natural -- ^ depth
               -> FileEntry
               -> Html
fileFormatList baseUrl depth FileEntry { fileIdentifier, fileFormats } = do
  "[ "
  toMarkup $ intersperse " | " $ fileFormat <$> fileFormats
  " ]"
  where
    fileFormat FileFormat { fileFormatName, fileFormatExtension } =
      a ! hrefWithDepth baseUrl depth (textValue $ "files/" <> fileIdentifier <> maybe "" ("." <>) fileFormatExtension) $
        toMarkup fileFormatName
