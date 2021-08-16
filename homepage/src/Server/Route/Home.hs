module Server.Route.Home where

handler :: Monad m
        => m String
handler = return ""
