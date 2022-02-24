module Homepage.Server.Route where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Server.Route.Blog qualified
import Homepage.Server.Route.Donate qualified
import Homepage.Server.Route.Files qualified
import Homepage.Server.Route.Home qualified
import Homepage.Server.Route.Static qualified

import Control.Monad.Logger
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data Routes route = Routes
    { routeHome :: route :- Homepage.Server.Route.Home.API
    , routeBlog :: route :- "blog" :> NamedRoutes Homepage.Server.Route.Blog.Routes
    , routeDonate :: route :- "donate" :> NamedRoutes Homepage.Server.Route.Donate.Routes
    , routeFiles :: route :- "files" :> NamedRoutes Homepage.Server.Route.Files.Routes
    , routeStatic :: route :- Homepage.Server.Route.Static.API
    }
  deriving stock Generic

routes :: (MonadBlog m, MonadConfigured m, MonadLogger m)
       => Routes (AsServerT m)
routes = Routes
    { routeHome = Homepage.Server.Route.Home.handler
    , routeBlog = Homepage.Server.Route.Blog.routes
    , routeDonate = Homepage.Server.Route.Donate.routes
    , routeFiles = Homepage.Server.Route.Files.routes
    , routeStatic = Homepage.Server.Route.Static.handler
    }
