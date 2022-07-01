module Homepage.Server.Route.Redirect where

import Homepage.Server.Route.Blog.Type qualified
import Homepage.Server.Route.Redirect.Type
import Homepage.Server.Route.Type qualified

import Servant
import Servant.Server.Generic

routes :: Applicative m => Routes (AsServerT m)
routes =
  Routes
    { routeFeed = pure $ headers $ show $ linkURI blogFeedLink
    , routeFeedXml = pure $ headers $ show $ linkURI blogFeedLink
    , routeRss = pure $ headers $ show $ linkURI blogFeedLink
    , routeRssXml = pure $ headers $ show $ linkURI blogFeedLink
    , routeAtom = pure $ headers $ show $ linkURI blogFeedLink
    , routeAtomXml = pure $ headers $ show $ linkURI blogFeedLink
    , routeBlogFeed = pure $ headers $ show $ linkURI feedLink
    , routeBlogFeedXml = pure $ headers $ show $ linkURI feedLink
    , routeBlogRss = pure $ headers $ show $ linkURI feedLink
    , routeBlogRssXml = pure $ headers $ show $ linkURI feedLink
    , routeBlogAtom = pure $ headers $ show $ linkURI feedLink
    }
 where
  headers :: String -> Found302Content
  headers loc =
    Headers
      { getResponse = NoContent
      , getHeadersHList = HCons (Header loc) HNil
      }
  blogFeedLink = Homepage.Server.Route.Blog.Type.routeFeed . Homepage.Server.Route.Type.routeBlog $ allFieldLinks
  feedLink = Homepage.Server.Route.Blog.Type.routeFeed allFieldLinks
