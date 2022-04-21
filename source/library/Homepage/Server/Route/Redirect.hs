module Homepage.Server.Route.Redirect where

import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic

type Found302Content = Headers '[Header "Location" String] NoContent
type Found302 = Verb 'GET 302 '[PlainText] Found302Content

data Routes route = Routes
  { routeFeed :: route :- "feed" :> Found302
  , routeFeedXml :: route :- "feed.xml" :> Found302
  , routeRss :: route :- "rss" :> Found302
  , routeRssXml :: route :- "rss.xml" :> Found302
  , routeAtom :: route :- "atom" :> Found302
  , routeAtomXml :: route :- "atom.xml" :> Found302
  , routeBlogFeed :: route :- "blog" :> "feed" :> Found302
  , routeBlogFeedXml :: route :- "blog" :> "feed.xml" :> Found302
  , routeBlogRss :: route :- "blog" :> "rss" :> Found302
  , routeBlogRssXml :: route :- "blog" :> "rss.xml" :> Found302
  , routeBlogAtom :: route :- "blog" :> "atom" :> Found302
  }
  deriving stock (Generic)

routes :: Applicative m => Routes (AsServerT m)
routes =
  Routes
    { routeFeed = pure $ headers "./blog/atom.xml"
    , routeFeedXml = pure $ headers "./blog/atom.xml"
    , routeRss = pure $ headers "./blog/atom.xml"
    , routeRssXml = pure $ headers "./blog/atom.xml"
    , routeAtom = pure $ headers "./blog/atom.xml"
    , routeAtomXml = pure $ headers "./blog/atom.xml"
    , routeBlogFeed = pure $ headers "./atom.xml"
    , routeBlogFeedXml = pure $ headers "./atom.xml"
    , routeBlogRss = pure $ headers "./atom.xml"
    , routeBlogRssXml = pure $ headers "./atom.xml"
    , routeBlogAtom = pure $ headers "./atom.xml"
    }
 where
  headers :: String -> Found302Content
  headers loc =
    Headers
      { getResponse = NoContent
      , getHeadersHList = HCons (Header loc) HNil
      }
