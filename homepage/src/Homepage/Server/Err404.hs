module Homepage.Server.Err404 where

import Servant
import Network.HTTP.Types.Status
import Network.Wai

application404 :: Application
application404 _ rsp = rsp $
    responseBuilder status404 [] "You got lost you dumbass." -- TODO
