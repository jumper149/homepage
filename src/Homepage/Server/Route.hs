module Homepage.Server.Route where

import           Homepage.Configuration
import qualified Homepage.Server.Route.Blog
import qualified Homepage.Server.Route.Donate
import qualified Homepage.Server.Route.Files
import qualified Homepage.Server.Route.Home
import qualified Homepage.Server.Route.Projects
import qualified Homepage.Server.Route.Static

import Control.Monad.Base
import Control.Monad.Error.Class
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data Routes route = Routes
    { routeHome :: route :- Homepage.Server.Route.Home.API
    , routeBlog :: route :- "blog" :> Homepage.Server.Route.Blog.API
    , routeDonate :: route :- "donate" :> Homepage.Server.Route.Donate.API
    , routeFiles :: route :- "files" :> Homepage.Server.Route.Files.API
    , routeProjects :: route :- "projects" :> Homepage.Server.Route.Projects.API
    , routeStatic :: route :- Homepage.Server.Route.Static.API
    }
  deriving stock (Generic)

routes :: (MonadBase IO m, MonadConfigured m, MonadError ServerError m)
       => Routes (AsServerT m)
routes = Routes
    { routeHome = Homepage.Server.Route.Home.handler
    , routeBlog = Homepage.Server.Route.Blog.handler
    , routeDonate = Homepage.Server.Route.Donate.handler
    , routeFiles = Homepage.Server.Route.Files.handler
    , routeProjects = Homepage.Server.Route.Projects.handler
    , routeStatic = Homepage.Server.Route.Static.handler
    }
