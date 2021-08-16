module Homepage where

import           Server.Err404
import qualified Server.Route.Home

import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Network.Wai.Handler.Warp
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

data Configuration = Configuration
    { configDirectoryBlog :: FilePath
    , configDirectoryFiles :: FilePath
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

-- TODO: Serve HTML.
-- TODO: Serve favicon and CSS.
data Routes route = Routes
    { routeHome :: route :- Get '[JSON] String
    , routeBlog :: route :- "blog" :> (Get '[JSON] String :<|> Raw)
    , routeDonate :: route :- "donate" :> (Get '[JSON] String :<|> "thankYou" :> Get '[JSON] String)
    , routeFiles :: route :- "files" :> (Get '[JSON] String :<|> Raw)
    , routeProjects :: route :- "projects" :> Get '[JSON] String
    }
  deriving stock (Generic)

-- TODO: Serve HTML.
routes :: Routes AsServer
routes = Routes
    { routeHome = Server.Route.Home.handler
    , routeBlog = return "blog" :<|> serveDirectoryWith (defaultFileServerSettings ".") { ss404Handler = Just application404 } -- TODO: Use 'Configuration'.
    , routeDonate = return "donate" :<|> return "donate/thankYou"
    , routeFiles = return "files" :<|> undefined -- TODO
    , routeProjects = return "projects"
    }

main :: IO ()
main = run 8080 $ genericServe routes
