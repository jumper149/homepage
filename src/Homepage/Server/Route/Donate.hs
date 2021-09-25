module Homepage.Server.Route.Donate where

import Homepage.Configuration
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

type API = Get '[HTML] Html
      :<|> "thankYou" :> Get '[HTML] Html

handler :: MonadConfigured m => ServerT API m
handler = donateHandler
     :<|> thankYouHandler

donateHandler :: MonadConfigured m
              => m Html
donateHandler = do
  baseUrl <- configBaseUrl <$> configuration
  pure $ document baseUrl (Just 0) Nothing $ do
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
    let paypalUrl = "https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=6BWNN8HJXF88L&source=url"
    H.div ! class_ "center" $
      b $ do
        a ! href paypalUrl $ "Donate"
        " via PayPal."
    br
    H.div ! class_ "center" $
      a ! href paypalUrl $
        img ! alt "QR-Code to donate via PayPal"
            ! src (withDepth baseUrl (Just 0) "donatePayPalQR.png")
            ! HA.style "width:128px;height:128px;"

-- TODO: Make sure that people are actually redirected here.
thankYouHandler :: MonadConfigured m
              => m Html
thankYouHandler = do
  baseUrl <- configBaseUrl <$> configuration
  pure $ document baseUrl (Just 1) Nothing $ do
    h2 "Thank you"
    p "I just want to let you know, that you are an awesome human being and I am very grateful for your support!"
    p ":)"
