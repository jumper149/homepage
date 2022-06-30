module Homepage.Server.Route.Blog.Type where

import Homepage.Configuration.Blog
import Homepage.Server.Route.Blog.Atom.Type qualified as Atom

import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM.Server qualified as RawM
import Text.Blaze.Html5

data Routes route = Routes
  { routeRaw :: route :- "raw" :> RawM.RawM
  , routeFeed :: route :- "atom.xml" :> Atom.API
  , routeArticle ::
      route
        :- Capture "article" BlogId
        :> UVerb 'GET '[HTML] '[WithStatus 200 Html, WithStatus 404 Html]
  , routeOverview :: route :- Get '[HTML] Html
  }
  deriving stock (Generic)
