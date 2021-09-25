module Homepage where

import Homepage.Application
import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Route

import Control.Monad.Trans.Control
import Network.Wai.Handler.Warp
import Servant.Server.Generic

main :: IO ()
main = runApplication $ do
  port <- configPort <$> configuration
  liftWith $ \ runT ->
    run (fromEnum port) $ genericServeT runT routes
