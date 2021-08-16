module Homepage.Server.Route.Projects where

import Servant

type API = Get '[JSON] String

handler :: Monad m
        => ServerT API m
handler = pure "projects"
