module Homepage.Server.Route.Home where

import Homepage.Server.Html.Header

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

type API = Get '[HTML] Html

handler :: Monad m
        => ServerT API m
handler = pure $
  docTypeHtml ! lang "en" $ do
      H.head $ do
          meta ! charset "UTF-8"
          meta ! name "author" ! content "Felix Springer"
          meta ! name "description" ! content "I am some guy, live somewhere, like some stuff, spend some time, have some projects and you want to know about me."
          meta ! name "viewport" ! content "width=500"
          H.title "Felix Springer's Homepage"
          link ! rel "icon" ! href "favicon.png"
          link ! rel "stylesheet" ! type_ "text/css" ! href "stylesheet.css"
      body $ do
          headerTabs $ Just TabHome
          img ! src "files/portrait.jpg" ! class_ "portrait" ! alt "Portrait of Felix Springer"
          h1 "Felix Springer"
          h2 "Welcome"
          p $ do
              "I am living in Germany and working as a Software Engineer at"
              a ! href "https://www.possehl-analytics.com/" $ "Possehl Analytics"
              "in Augsburg."
          p $ do
              "I recently completed my Bachelor's degree in Physics with a minor in Computer Science, which I studied for at"
              a ! href "https://www.uni-hannover.de/en/" $ "Leibniz Universität Hannover"
              "."
          p "In my free time I like messing around with Linux and programming.\n        I am spending most of my time with functional languages like Haskell.\n        If I want to take a break from the computer I enjoy playing guitar or badminton.\n        I also love green tea."
          h2 "current Projects"
          ul $ do
              li $ do
                  "A blue light filter in the style of XMonad:"
                  a ! href "https://github.com/jumper149/blucontrol" $ "blucontrol"
                  "replaces blugon for my personal usage"
              li $ do
                  "The game"
                  a ! href "https://github.com/jumper149/go" $ "go"
                  "providing a smooth multiplayer browser experience with a webserver"
              li $ do
                  "A simple Blue Light Filter for X:"
                  a ! href "https://github.com/jumper149/blugon" $ "blugon"
              li $ do
                  "The"
                  a ! href "https://github.com/jumper149/dotfiles" $ "dotfiles"
                  "to configure my ArchLinux-Systems"
          h2 "recent Blog"
          ul $ do
              li $ do
                  "30/06/19 -"
                  a ! href "blog/myOwnImplementationOfIExpressions.html" $ "my own Implementation of I-Expressions"
              li $ do
                  "04/04/19 -"
                  a ! href "blog/myWayToCoreboot.html" $ "my Way to Coreboot"
          h2 "shared Files"
          p $ do
              "You can download some of my shared files"
              a ! href "files.html" $ "here"
              "."
          h2 "Contact"
          ul $ do
              li $ do
                  a ! href "mailto:felixspringer@gmail.com" $ "E-Mail"
                  ": felixspringer149@gmail.com"
              li $ a ! href "https://github.com/jumper149" $ "GitHub"
          h2 "Donate"
          p $ do
              "If you want to support me, you can donate to me"
              a ! href "donate/donate.html" $ "here"
              "."
