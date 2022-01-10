module Homepage.Server where

import Homepage.Application
import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Route

import Control.Monad.Trans.Control
import Network.Wai.Handler.Warp
import Servant.Server.Generic

server :: ApplicationT IO ()
server = do
  port <- configPort <$> configuration
  liftWith $ \ runT ->
    run (fromEnum port) $ genericServeT runT routes
