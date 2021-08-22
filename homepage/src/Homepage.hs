module Homepage where

import Homepage.Configuration
import Homepage.CLI
import Homepage.Server.Route

import Servant.Server.Generic
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  config <- launch
  let runStack :: ConfiguredT m a -> m a
      runStack x = runConfiguredT x config
  run 8081 $ genericServeT runStack routes
