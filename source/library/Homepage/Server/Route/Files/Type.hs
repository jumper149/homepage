module Homepage.Server.Route.Files.Type where

import Servant.API
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM qualified as RawM
import Text.Blaze.Html5

data Routes route = Routes
  { routeOverview :: route :- Get '[HTML] Html
  , routeFiles :: route :- RawM.RawM
  }
  deriving stock (Generic)
