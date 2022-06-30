module Homepage.Server.Route.Donate.Type where

import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Text.Blaze.Html5

data Routes route = Routes
  { routeDonate :: route :- Get '[HTML] Html
  , routeThankYou :: route :- "thankYou" :> Get '[HTML] Html
  }
  deriving stock (Generic)
