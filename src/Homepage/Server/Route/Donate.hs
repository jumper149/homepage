{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Donate where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Logger
import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.Server.Generic
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

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
  $logInfo "Serve donation page."
  pure $ document baseUrl contactInformation revision (Just 0) Nothing $ do
    h2 "Donate to me"
    h3 "Reasons to donate"
    ul $ do
       li $ do
         "You like one of my "
         i "projects"
       li $ do
         "You like my "
         i "blog"
       li $ do
         "You want to support "
         i "Free"
         " and "
         i "Open Source"
         " Software"
       li $ do
         "You just think I'm a "
         i "nice"
         " person"
       li $ do
         "You have big "
         i "PP"
    hr
    br
    case maybeDonateInformation of
      Nothing -> p $ do
        "Actually there is no way to "
        i "donate"
        " to me at the moment."
      Just donateInformation -> do
        let paypalUrl = donatePaypalUrl donateInformation
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

thankYouHandler :: (MonadConfigured m, MonadLogger m)
                => m Html
thankYouHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  $logInfo "Serve thankful donation page."
  pure $ document baseUrl contactInformation revision (Just 1) Nothing $ do
    h2 "Thank you"
    p "I just want to let you know, that you are an awesome human being and I am very grateful for your support!"
    p ":)"
