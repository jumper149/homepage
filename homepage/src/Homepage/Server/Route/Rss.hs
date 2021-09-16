module Homepage.Server.Route.Rss where

import Homepage.Blog
import Homepage.Configuration

import qualified Data.Text as T
import qualified Text.Atom.Feed as Atom
import Servant

type API = Get '[PlainText] T.Text

handler :: MonadConfigured m
        => ServerT API m
handler = do
  blogs <- configBlogEntries <$> configuration
  undefined

atomEntry :: T.Text -- ^ URI
          -> BlogEntry
          -> Atom.Entry
atomEntry uri BlogEntry { blogContent , blogTitle , blogTimestamp } = Atom.Entry
  { Atom.entryId = uri
  , Atom.entryTitle = Atom.TextString blogTitle
  , Atom.entryUpdated = undefined blogTimestamp -- TODO
  , Atom.entryAuthors = [ personFelixSpringer ]
  , Atom.entryCategories = undefined
  , Atom.entryContent = undefined
  , Atom.entryContributor = undefined
  , Atom.entryLinks = undefined
  , Atom.entryPublished = Nothing
  , Atom.entryRights = undefined
  , Atom.entrySource = undefined
  , Atom.entrySummary = undefined
  , Atom.entryInReplyTo = Nothing
  , Atom.entryInReplyTotal = Nothing
  , Atom.entryAttrs = undefined
  , Atom.entryOther = []
  }

personFelixSpringer :: Atom.Person
personFelixSpringer = Atom.Person
  { Atom.personName = "Felix Springer"
  , Atom.personURI = Just "https://felixspringer.xyx"
  , Atom.personEmail = Just "felixspringer149@gmail.com"
  , Atom.personOther = []
  }
