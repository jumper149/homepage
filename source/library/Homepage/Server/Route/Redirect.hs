module Homepage.Server.Route.Redirect where

import Homepage.Server.Route.Redirect.Type

import Servant
import Servant.Server.Generic

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
