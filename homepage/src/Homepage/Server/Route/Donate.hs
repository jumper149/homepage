module Homepage.Server.Route.Donate where

import Servant

type API = Get '[JSON] String
      :<|> "thankYou" :> Get '[JSON] String

handler :: Monad m => ServerT API m
handler = pure "donate"
     :<|> pure "donate/thankYou"
