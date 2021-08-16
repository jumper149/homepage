module Homepage.Server.Route.Home where

import Servant

type API = Get '[JSON] String

handler :: Monad m
        => ServerT API m
handler = pure ""
