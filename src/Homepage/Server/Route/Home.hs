{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Home where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog
import Homepage.Configuration.Contact
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import Homepage.Server.Tab

import Control.Monad.Logger
import Data.Maybe
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
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  blogs <- configBlogEntries <$> configuration
  blogPreviewMaxLength <- configBlogPreviewMaxLength <$> configuration
  $logInfo "Serve main page."
  pure $ document baseUrl contactInformation revision (Just 0) (Just TabHome) $ do
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
    contactHtml contactInformation
    h2 "Donate"
    p $ do
        "If you want to support me, you can donate to me "
        a ! hrefWithDepth baseUrl (Just 0) "donate" $ "here"
        "."

contactHtml :: ContactInformation -> Html
contactHtml ContactInformation
  { contactEmailAddress
  , contactMatrix
  , contactLiberaChat
  , contactGithubUsername
  , contactGitlabUsername
  , contactHackageUsername
  , contactAurUsername
  } = ul $ toMarkup $ li <$> catMaybes
    [ markupEmailAddress <$> contactEmailAddress
    , markupMatrix <$> contactMatrix
    , markupLiberaChat <$> contactLiberaChat
    , markupGitHub <$> contactGithubUsername
    , markupGitLab <$> contactGitlabUsername
    , markupHackage <$> contactHackageUsername
    , markupAUR <$> contactAurUsername
    ]
    where
      markupEmailAddress emailAddress = do
        a ! href ("mailto:" <> textValue emailAddress) $ "E-Mail"
        ": " <> toMarkup emailAddress
      markupMatrix matrix = toMarkup $ "Matrix: " <> matrix
      markupLiberaChat liberaChat = "IRC (Libera Chat): " <> toMarkup liberaChat
      markupGitHub githubUsername = toMarkup $ do
        a ! href ("https://github.com/" <> textValue githubUsername) $ "GitHub"
        ": " <> toMarkup githubUsername
      markupGitLab gitlabUsername = toMarkup $ do
        a ! href ("https://gitlab.com/" <> textValue gitlabUsername) $ "GitLab"
        ": " <> toMarkup gitlabUsername
      markupHackage hackageUsername = do
        a ! href ("https://hackage.haskell.org/user/" <> textValue hackageUsername) $ "Hackage"
        ": " <> toMarkup hackageUsername
      markupAUR aurUsername = do
        a ! href ("https://aur.archlinux.org/packages/?K=" <> textValue aurUsername <> "&SeB=m") $ "AUR"
        ": " <> toMarkup aurUsername
