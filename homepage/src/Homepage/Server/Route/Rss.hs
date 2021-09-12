module Homepage.Server.Route.Rss where

import qualified Data.Text as T
import Servant

type API = Get '[PlainText] T.Text

handler :: Monad m => ServerT API m
handler = undefined
