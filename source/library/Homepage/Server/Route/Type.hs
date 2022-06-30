module Homepage.Server.Route.Type where

import Homepage.Server.Route.Blog qualified
import Homepage.Server.Route.Donate qualified
import Homepage.Server.Route.Files qualified
import Homepage.Server.Route.Home qualified
import Homepage.Server.Route.Redirect qualified
import Homepage.Server.Route.Static qualified

import GHC.Generics
import Servant
import Servant.API.Generic

data Routes route = Routes
  { routeHome :: route :- Homepage.Server.Route.Home.API
  , routeBlog :: route :- "blog" :> NamedRoutes Homepage.Server.Route.Blog.Routes
  , routeDonate :: route :- "donate" :> NamedRoutes Homepage.Server.Route.Donate.Routes
  , routeFiles :: route :- "files" :> NamedRoutes Homepage.Server.Route.Files.Routes
  , routeRedirect :: route :- NamedRoutes Homepage.Server.Route.Redirect.Routes
  , routeStatic :: route :- Homepage.Server.Route.Static.API
  }
  deriving stock (Generic)
