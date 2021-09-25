module Homepage.Server.Err404 where

import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Error.Class
import Servant
import Network.HTTP.Types.Status
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H

application404 :: Application
application404 _ rsp = rsp $
  responseBuilder status404 [] "You got lost you dumbass." -- TODO

servantError404 :: (MonadConfigured m, MonadError ServerError m)
                => m a
servantError404 = do
  baseUrl <- configBaseUrl <$> configuration
  throwError err404
    { errHeaders = [ (,) "Content-Type" "text/html" ]
    , errBody = renderHtml $
        document baseUrl Nothing Nothing $ do
          h1 "404"
          h2 "You got lost?"
          p $ "My homepage is " <> (a ! hrefWithDepth baseUrl Nothing "" $ "here") <> "."
    }
