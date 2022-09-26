module Homepage.Server.Html.Files where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Files
import Homepage.Server.Html.Depth

import Data.Foldable
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Calendar
import Numeric.Natural
import Text.Blaze.Html5

fileList ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  FileEntries ->
  Html
fileList baseUrl depth files = do
  markupEntries topLevelEntries
  traverse_ markupSection $ M.toList otherEntries
 where
  entryGroups = groupFiles $ unFileEntries files
  topLevelEntries = fromMaybe mempty $ M.lookup Nothing entryGroups
  otherEntries = M.mapKeys fromJust . M.delete Nothing $ entryGroups
  markupSection (sectionName, entrySet) = do
    h2 $ text $ "my " <> sectionName
    markupEntries entrySet
  markupEntries entrySet = ul $ toMarkup $ fileToMarkup <$> L.sortOn (Down . fileTimestamp) (S.toList entrySet)
  fileToMarkup file@FileEntry {fileName, fileTimestamp} =
    li $ do
      toMarkup $ T.pack (showGregorian fileTimestamp)
      " - "
      toMarkup fileName
      " "
      fileFormatList baseUrl depth file

fileFormatList ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  FileEntry ->
  Html
fileFormatList baseUrl depth FileEntry {fileIdentifier, fileFormats} = do
  "[ "
  toMarkup $ L.intersperse " | " $ fileFormat <$> fileFormats
  " ]"
 where
  fileFormat FileFormat {fileFormatName, fileFormatExtension} =
    a ! hrefWithDepth baseUrl depth (textValue $ "files/" <> fileIdentifier <> maybe "" ("." <>) fileFormatExtension) $ -- TODO: Use `Servant.Links`.
      toMarkup fileFormatName
