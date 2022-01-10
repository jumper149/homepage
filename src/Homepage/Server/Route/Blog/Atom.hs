{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Blog.Atom where

import Homepage.Application.Blog
import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Blog

import Control.Monad.Error.Class
import Control.Monad.Logger
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Text as T
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

handler :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
        => ServerT API m
handler = do
  blogs <- configBlogEntries <$> configuration
  atomMaxLength <- configAtomMaxLength <$> configuration
  let recentBlogs = recentBlogEntries atomMaxLength blogs
  let entryList = sortOn (Down . blogTimestamp . snd) $ M.toList (unBlogEntries recentBlogs)
  entries <- traverse (uncurry atomEntry) entryList
  feed <- Feed.AtomFeed <$> atomFeed entries
  case Feed.textFeed feed of
    Nothing -> do
      $logError "Failed to serve Atom feed."
      throwError err500 { errBody = "Failed to serialize feed." }
    Just text -> do
      $logInfo "Serve Atom feed."
      pure $ AtomFeed text

atomFeed :: MonadConfigured m
         => [Atom.Entry]
         -> m Atom.Feed
atomFeed entries = do
  baseUrl <- configBaseUrl <$> configuration
  personName <- configAtomPersonName <$> configuration
  person <- atomPerson
  pure Atom.Feed
    { Atom.feedId = displayBaseUrl baseUrl <> "blog/atom.xml"
    , Atom.feedTitle = Atom.TextString $ personName <> "'s Blog"
    , Atom.feedUpdated = case entries of
        [] -> "1997-09-14T12:00:00+01:00" -- This is just a fallback in case there aren't any entries.
        Atom.Entry { Atom.entryUpdated } : _ -> entryUpdated -- Expects sorted entries.
    , Atom.feedAuthors = [ person ]
    , Atom.feedCategories = []
    , Atom.feedContributors = []
    , Atom.feedGenerator = Just Atom.Generator
        { Atom.genURI = Just "https://github.com/jumper149/homepage"
        , Atom.genVersion = Nothing
        , Atom.genText = "Felix Springer's Homepage"
        }
    , Atom.feedIcon = Just $ displayBaseUrl baseUrl <> "favicon.png"
    , Atom.feedLinks =
        [ Atom.Link
            { Atom.linkHref = displayBaseUrl baseUrl <> "blog/atom.xml"
            , Atom.linkRel = Just $ Left "self"
            , Atom.linkType = Nothing -- TODO
            , Atom.linkHrefLang = Nothing
            , Atom.linkTitle = Nothing -- TODO
            , Atom.linkLength = Nothing
            , Atom.linkAttrs = []
            , Atom.linkOther = []
            }
        , Atom.Link
            { Atom.linkHref = displayBaseUrl baseUrl <> "blog"
            , Atom.linkRel = Nothing
            , Atom.linkType = Nothing -- TODO
            , Atom.linkHrefLang = Nothing
            , Atom.linkTitle = Nothing -- TODO
            , Atom.linkLength = Nothing
            , Atom.linkAttrs = []
            , Atom.linkOther = []
            }
        ]
    , Atom.feedLogo = Nothing
    , Atom.feedRights = Just $ Atom.TextString "Attribution 4.0 International (CC BY 4.0)"
    , Atom.feedSubtitle = Nothing
    , Atom.feedEntries = entries
    , Atom.feedAttrs = []
    , Atom.feedOther = []
    }

atomEntry :: (MonadBlog m, MonadConfigured m, MonadLogger m)
          => BlogId
          -> BlogEntry
          -> m Atom.Entry
atomEntry blogId BlogEntry { blogTitle , blogTimestamp } = do
  baseUrl <- configBaseUrl <$> configuration
  person <- atomPerson
  $logInfo $ "Read blog article from file: " <> T.pack (show blogId)
  content <- readBlogEntryHtml blogId
  pure Atom.Entry
    { Atom.entryId = displayBaseUrl baseUrl <> "blog/" <> unBlogId blogId
    , Atom.entryTitle = Atom.TextString blogTitle
    , Atom.entryUpdated = T.pack $ T.formatTime T.defaultTimeLocale "%0Y-%0m-%0dT12:00:00+01:00" blogTimestamp
    , Atom.entryAuthors = [ person ]
    , Atom.entryCategories = []
    , Atom.entryContent = Just $ Atom.HTMLContent content
    , Atom.entryContributor = []
    , Atom.entryLinks =
        [ Atom.Link
            { Atom.linkHref = displayBaseUrl baseUrl <> "blog/" <> unBlogId blogId
            , Atom.linkRel = Nothing
            , Atom.linkType = Nothing -- TODO
            , Atom.linkHrefLang = Nothing
            , Atom.linkTitle = Nothing -- TODO
            , Atom.linkLength = Nothing
            , Atom.linkAttrs = []
            , Atom.linkOther = []
            }
        ]
    , Atom.entryPublished = Nothing
    , Atom.entryRights = Nothing
    , Atom.entrySource = Nothing
    , Atom.entrySummary = Nothing
    , Atom.entryInReplyTo = Nothing
    , Atom.entryInReplyTotal = Nothing
    , Atom.entryAttrs = []
    , Atom.entryOther = []
    }

atomPerson :: MonadConfigured m
           => m Atom.Person
atomPerson = do
  baseUrl <- configBaseUrl <$> configuration
  personName <- configAtomPersonName <$> configuration
  maybePersonEmail <- configAtomPersonEmail <$> configuration
  pure Atom.Person
    { Atom.personName = personName
    , Atom.personURI = Just $ displayBaseUrl baseUrl
    , Atom.personEmail = maybePersonEmail
    , Atom.personOther = []
    }
