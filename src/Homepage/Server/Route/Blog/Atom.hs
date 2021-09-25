module Homepage.Server.Route.Blog.Atom where

import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration

import Control.Monad.Base
import Control.Monad.Error.Class
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Time as T
import GHC.Generics
import qualified Network.HTTP.Media as Media
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Export as Feed
import Servant

type API = Get '[Atom] AtomFeed

data Atom

instance Accept Atom where
  contentType _ = "application" Media.// "atom+xml"

newtype AtomFeed = AtomFeed { unAtomFeed :: LT.Text }
  deriving (Eq, Generic, Ord, Read, Show)

instance MimeRender Atom AtomFeed where
  mimeRender _ (AtomFeed text) = LT.encodeUtf8 text

handler :: (MonadBase IO m, MonadConfigured m, MonadError ServerError m)
        => ServerT API m
handler = do
  blogs <- configBlogEntries <$> configuration
  let entryList = take 20 $ sortOn (Down . blogTimestamp . snd) $ M.toList (unBlogEntries blogs)
  entries <- traverse (uncurry atomEntry) entryList
  feed <- Feed.AtomFeed <$> atomFeed entries
  case Feed.textFeed feed of
    Nothing -> throwError err500 { errBody = "Failed to serialize feed." }
    Just text -> pure $ AtomFeed text

atomFeed :: MonadConfigured m
         => [Atom.Entry]
         -> m Atom.Feed
atomFeed entries = do
  baseUrl <- configBaseUrl <$> configuration
  pure Atom.Feed
    { Atom.feedId = baseUrl <> "/blog/atom.xml"
    , Atom.feedTitle = Atom.TextString "Felix Springer's Blog"
    , Atom.feedUpdated = case entries of
        [] -> "1997-09-14T12:00:00+01:00"
        Atom.Entry { Atom.entryUpdated } : _ -> entryUpdated -- Expects sorted entries.
    , Atom.feedAuthors = [ personFelixSpringer ]
    , Atom.feedCategories = []
    , Atom.feedContributors = []
    , Atom.feedGenerator = Just Atom.Generator
        { Atom.genURI = Just "https://github.com/jumper149/homepage"
        , Atom.genVersion = Nothing
        , Atom.genText = "Felix Springer's Homepage"
        }
    , Atom.feedIcon = Just $ baseUrl <> "/favicon.png"
    , Atom.feedLinks = []
    , Atom.feedLogo = Nothing
    , Atom.feedRights = Just $ Atom.TextString "Attribution 4.0 International (CC BY 4.0)"
    , Atom.feedSubtitle = Nothing
    , Atom.feedEntries = entries
    , Atom.feedAttrs = []
    , Atom.feedOther = []
    }

atomEntry :: (MonadBase IO m, MonadConfigured m)
          => T.Text -- ^ Blog ID
          -> BlogEntry
          -> m Atom.Entry
atomEntry blogId BlogEntry { blogContent , blogTitle , blogTimestamp } = do
  baseUrl <- configBaseUrl <$> configuration
  dir <- configDirectoryBlog <$> configuration
  content <- liftBase $ T.readFile $ dir <> "/" <> T.unpack blogContent <> ".html"
  pure Atom.Entry
    { Atom.entryId = baseUrl <> "/blog/" <> blogId
    , Atom.entryTitle = Atom.TextString blogTitle
    , Atom.entryUpdated = T.pack $ T.formatTime T.defaultTimeLocale "%0Y-%0m-%0dT12:00:00+01:00" blogTimestamp
    , Atom.entryAuthors = [ personFelixSpringer ]
    , Atom.entryCategories = []
    , Atom.entryContent = Just $ Atom.HTMLContent content
    , Atom.entryContributor = []
    , Atom.entryLinks = []
    , Atom.entryPublished = Nothing
    , Atom.entryRights = Nothing
    , Atom.entrySource = Nothing
    , Atom.entrySummary = Nothing
    , Atom.entryInReplyTo = Nothing
    , Atom.entryInReplyTotal = Nothing
    , Atom.entryAttrs = []
    , Atom.entryOther = []
    }

personFelixSpringer :: Atom.Person
personFelixSpringer = Atom.Person
  { Atom.personName = "Felix Springer"
  , Atom.personURI = Just "https://felixspringer.xyz"
  , Atom.personEmail = Just "felixspringer149@gmail.com"
  , Atom.personOther = []
  }
