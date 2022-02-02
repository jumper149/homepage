module Homepage.Server.Err404 where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Data.ByteString.Builder
import Servant
import Network.HTTP.Types.Status
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H

application404 :: MonadConfigured m
               => m Application
application404 = do
  html404' <- html404
  pure $ \ _ rsp -> rsp $
    responseBuilder status404 [ (,) "Content-Type" "text/html" ] $
      lazyByteString $ renderHtml html404'

html404 :: MonadConfigured m
        => m Html
html404 = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  pure $ document baseUrl contactInformation revision Nothing Nothing $ do
    h1 "404"
    h2 "You got lost?"
    p $ "My homepage is " <> (a ! hrefWithDepth baseUrl Nothing "" $ "here") <> "."
