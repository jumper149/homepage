{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Donate where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Logger
import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.Server.Generic
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

data Routes route = Routes
    { routeDonate :: route
                  :- Get '[HTML] Html
    , routeThankYou :: route
                    :- "thankYou"
                    :> Get '[HTML] Html
    }
  deriving Generic

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
    -- TODO: This URL includes redirection after donation and should be configured instead of hardcoded.
    let paypalUrl = "https://www.paypal.com/donate?hosted_button_id=3LZQ9DCDDFFWU"
    H.div ! HA.style "text-align: center;" $
      b $ do
        a ! href paypalUrl $ "Donate"
        " via PayPal."
    br
    H.div ! HA.style "text-align: center;" $
      a ! href paypalUrl $
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
