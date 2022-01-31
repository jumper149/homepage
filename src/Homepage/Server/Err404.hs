module Homepage.Server.Err404 where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Error.Class
import Data.ByteString.Builder
import qualified Data.Text as T
import Servant
import Network.HTTP.Types.Status
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H

application404 :: MonadConfigured m
               => m Application
application404 = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  pure $ \ _ rsp -> rsp $
    responseBuilder status404 [ (,) "Content-Type" "text/html" ] $
      lazyByteString $ renderHtml $ html404 baseUrl contactInformation revision

servantError404 :: (MonadConfigured m, MonadError ServerError m)
                => m a
servantError404 = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  throwError err404
    { errHeaders = [ (,) "Content-Type" "text/html" ]
    , errBody = renderHtml $ html404 baseUrl contactInformation revision
    }

html404 :: BaseUrl
        -> ContactInformation
        -> Maybe T.Text -- ^ revision
        -> Html
html404 baseUrl contactInformation revision =
  document baseUrl contactInformation revision Nothing Nothing $ do
    h1 "404"
    h2 "You got lost?"
    p $ "My homepage is " <> (a ! hrefWithDepth baseUrl Nothing "" $ "here") <> "."
