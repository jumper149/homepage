module Homepage.Server.Route.Donate where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Logger.CallStack
import Data.List (intersperse)
import Data.Maybe
import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.Server.Generic
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA

data Routes route = Routes
    { routeDonate :: route
                  :- Get '[HTML] Html
    , routeThankYou :: route
                    :- "thankYou"
                    :> Get '[HTML] Html
    }
  deriving stock Generic

routes :: (MonadConfigured m, MonadLogger m)
       => Routes (AsServerT m)
routes = Routes
    { routeDonate = donateHandler
    , routeThankYou = thankYouHandler
    }

donateHandler :: (MonadConfigured m, MonadLogger m)
              => m Html
donateHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  let maybeDonateInformation = contactDonateInformation contactInformation
  logInfo "Serve donation page."
  pure $ document baseUrl contactInformation revision (Just 0) Nothing $ do
    h2 "Donate to me"
    case maybeDonateInformation of
      Nothing -> p $ do
        "Actually there is no way to "
        i "donate"
        " to me at the moment."
      Just donateInformation -> do
        br
        donateHtml baseUrl donateInformation
        br

donateHtml :: BaseUrl -> DonateInformation -> Html
donateHtml baseUrl DonateInformation
  { donatePaypalUrl
  , donateXmrAddress
  } = toMarkup $ intersperse (hr >> br) $ catMaybes
    [ markupPaypalUrl <$> donatePaypalUrl
    , markupXmrAddress <$> donateXmrAddress
    ]
    where
      markupPaypalUrl paypalUrl = do
        H.div ! HA.style "text-align: center;" $
          b $ do
            a ! href (textValue paypalUrl) $ "Donate"
            " via PayPal."
        br
        H.div ! HA.style "text-align: center;" $
          -- TODO: Configure QR-Code.
          a ! href (textValue paypalUrl) $
            img ! alt "QR-Code to donate via PayPal"
                ! src (withDepth baseUrl (Just 0) "donatePayPalQR.png")
                ! HA.style "width: 128px; height: 128px;"
      markupXmrAddress xmrAddress =
        H.div ! HA.style "text-align: center;" $
          b $ do
            "Donate via XMR: "
            text xmrAddress

thankYouHandler :: (MonadConfigured m, MonadLogger m)
                => m Html
thankYouHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  logInfo "Serve thankful donation page."
  pure $ document baseUrl contactInformation revision (Just 1) Nothing $ do
    h2 "Thank you"
    p "I just want to let you know, that you are an awesome human being and I am very grateful for your support!"
    p ":)"
