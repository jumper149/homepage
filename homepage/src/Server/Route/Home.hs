module Server.Route.Home where

import Servant

handler :: Handler String
handler = return ""
