{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Home where

import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import Homepage.Server.Tab

import Control.Monad.Logger
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import Text.Blaze.Html5.Attributes

type API = Get '[HTML] Html

handler :: (MonadConfigured m, MonadLogger m)
        => ServerT API m
handler = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  blogPreviewMaxLength <- configBlogPreviewMaxLength <$> configuration
  $logInfo "Serve main page."
  pure $ document baseUrl (Just 0) (Just TabHome) $ do
    img ! src "portrait.jpg" ! class_ "portrait" ! alt "Portrait of Felix Springer"
    h1 "Felix Springer"
    h2 "Welcome"
    p $ do
        "I am living in Germany and working as a Software Engineer at "
        a ! href "https://www.possehl-analytics.com/" $ "Possehl Analytics"
        " in Augsburg."
    p $ do
        "I recently completed my Bachelor's degree in Physics with a minor in Computer Science, which I studied for at "
        a ! href "https://www.uni-hannover.de/en/" $ "Leibniz Universität Hannover"
        "."
    p $ do
      "In my free time I like messing around with Linux and programming.\n\
      \I am spending most of my time with functional languages like Haskell.\n\
      \If I want to take a break from the computer I enjoy playing guitar or badminton.\n\
      \I also love green tea."
    h2 "recent Blog"
    p $ do
      "You can stay up to date by subscribing to this "
      a ! hrefWithDepth baseUrl (Just 0) "blog/atom.xml" $ s "RSS" <> "/Atom Feed"
      "."
    blogList baseUrl (Just 0) $ recentBlogEntries blogPreviewMaxLength blogs
    p $ do
      "The full list of blog articles can be accessed "
      a ! hrefWithDepth baseUrl (Just 0) "blog" $ "here"
      "."
    h2 "shared Files"
    p $ do
        "You can download some of my shared files "
        a ! hrefWithDepth baseUrl (Just 0) "files" $ "here"
        "."
    h2 "Contact"
    ul $ do
        li $ do
            a ! href "mailto:felixspringer@gmail.com" $ "E-Mail"
            ": felixspringer149@gmail.com"
        li "Matrix: @jumper149:matrix.org"
        li "IRC (Libera Chat): jumper149"
        li $ do
          a ! href "https://github.com/jumper149" $ "GitHub"
          ": jumper149"
        li $ do
          a ! href "https://hackage.haskell.org/user/jumper149" $ "Hackage"
          ": jumper149"
        li $ do
          a ! href "https://aur.archlinux.org/packages/?K=jumper149&SeB=m" $ "AUR"
          ": jumper149"
    h2 "Donate"
    p $ do
        "If you want to support me, you can donate to me "
        a ! hrefWithDepth baseUrl (Just 0) "donate" $ "here"
        "."
