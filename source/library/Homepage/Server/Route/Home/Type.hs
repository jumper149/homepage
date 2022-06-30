module Homepage.Server.Route.Home.Type where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5

type API = Get '[HTML] Html
