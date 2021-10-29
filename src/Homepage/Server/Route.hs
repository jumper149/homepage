module Homepage.Server.Route where

import           Homepage.Application.Blog
import           Homepage.Application.Configured
import qualified Homepage.Server.Route.Blog
import qualified Homepage.Server.Route.Donate
import qualified Homepage.Server.Route.Files
import qualified Homepage.Server.Route.Home
import qualified Homepage.Server.Route.Projects
import qualified Homepage.Server.Route.Static

import Control.Monad.Error.Class
import Control.Monad.Logger
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data Routes route = Routes
    { routeHome :: route :- Homepage.Server.Route.Home.API
    , routeBlog :: route :- "blog" :> ToServantApi Homepage.Server.Route.Blog.Routes
    , routeDonate :: route :- "donate" :> ToServantApi Homepage.Server.Route.Donate.Routes
    , routeFiles :: route :- "files" :> ToServantApi Homepage.Server.Route.Files.Routes
    , routeProjects :: route :- "projects" :> Homepage.Server.Route.Projects.API
    , routeStatic :: route :- Homepage.Server.Route.Static.API
    }
  deriving stock (Generic)

routes :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
       => Routes (AsServerT m)
routes = Routes
    { routeHome = Homepage.Server.Route.Home.handler
    , routeBlog = toServant Homepage.Server.Route.Blog.routes
    , routeDonate = toServant Homepage.Server.Route.Donate.routes
    , routeFiles = toServant Homepage.Server.Route.Files.routes
    , routeProjects = Homepage.Server.Route.Projects.handler
    , routeStatic = Homepage.Server.Route.Static.handler
    }
