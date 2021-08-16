module Homepage where

import Homepage.Configuration
import Homepage.Server.Route

import Servant.Server.Generic
import Network.Wai.Handler.Warp

configurationDefault :: Configuration
configurationDefault = Configuration
    { configDirectoryBlog = "./blog"
    , configDirectoryFiles = "./files"
    }

main :: IO ()
main = run 8081 $ genericServeT runStack routes
  where runStack x = runConfiguredT x configurationDefault
