module Homepage.Server.Html.Files where

import Homepage.Files
import Homepage.Server.Html.Depth

import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe
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
fileList baseUrl depth files = do
  markupEntries topLevelEntries
  traverse_ markupSection $ M.toList otherEntries
  where
    entryGroups = groupFiles $ unFileEntries files
    topLevelEntries = fromMaybe mempty $ M.lookup Nothing entryGroups
    otherEntries = M.mapKeys fromJust . M.delete Nothing $ entryGroups
    markupSection (sectionName, entrySet) = do
      h2 $ toMarkup $ "my " <> sectionName
      markupEntries entrySet
    markupEntries entrySet = ul $ toMarkup $ fileToMarkup <$> sortOn (Down . fileTimestamp) (S.toList entrySet)
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
