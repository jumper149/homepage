{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Blog where

import Homepage.Application.Blog
import Homepage.Application.Configured
import Homepage.Blog
import Homepage.Configuration
import Homepage.Server.Err404
import Homepage.Server.Html.AsciiDoc
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import qualified Homepage.Server.Route.Blog.Atom as Atom
import Homepage.Server.Tab

import Control.Monad.Error.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Servant hiding (serveDirectoryWith)
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as H

type API = "atom.xml" :> Atom.API
      :<|> Capture "article" T.Text :>
        (    Get '[HTML] Html
        :<|> (    "html" :> Get '[HTML] Html
             :<|> "adoc" :> Get '[HTML] Html
             :<|> "pdf" :> Get '[HTML] Html
             )
        )
      :<|> Get '[HTML] Html

handler :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
        => ServerT API m
handler = Atom.handler
     :<|> articlesHandler
     :<|> overviewHandler

overviewHandler :: (MonadConfigured m, MonadLogger m)
                => m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  $logInfo "Serve blog overview."
  pure $ document baseUrl (Just 0) (Just TabBlog) $ do
    h2 "my Blog"
    p $ do
      "My blog is available as an "
      a ! hrefWithDepth baseUrl (Just 0) "blog/atom.xml" $ "Atom Feed"
      "."
    blogList baseUrl (Just 0) blogs

articlesHandler :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
                => T.Text
                -> (m Html :<|> m Html :<|> m Html :<|> m Html)
articlesHandler articleKey = wrapperHandler articleKey :<|> htmlHandler articleKey :<|> undefined :<|> undefined

wrapperHandler :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
               => T.Text
               -> m Html
wrapperHandler articleKey = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  case lookupBlog articleKey blogs of
    Nothing -> do
        $logError $ "Failed to serve blog article: " <> T.pack (show articleKey)
        servantError404
    Just blog -> do
        $logInfo $ "Serve blog article: " <> T.pack (show articleKey)
        pure $ document baseUrl (Just 1) (Just TabBlog) $ do
          h2 $ text $ blogTitle blog
          p $ do
            "View blog entry: "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/" <> articleKey <> "/html") $ "HTML"
            " | "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/" <> articleKey <> "/pdf") $ "PDF"
            " | "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/" <> articleKey <> "/adoc") $ "ADOC"
          p "Preview below."
          hr
          script ! type_ "text/javascript" $
            "function resizeIframe(iframe) {\
            \  iframe.height = iframe.contentWindow.document.body.scrollHeight + \"px\";\
            \}"
          iframe ! src (textValue $ "./" <> articleKey <> "/html")
                 ! name "blog article (HTML)"
                 ! width "100%"
                 ! H.onload "resizeIframe(this)"
                 ! target "_parent"
                 ! H.style "border: none;"
                 $ mempty

htmlHandler :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
            => T.Text
            -> m Html
htmlHandler articleKey = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  case lookupBlog articleKey blogs of
    Nothing -> do
        $logError $ "Failed to serve blog article: " <> T.pack (show articleKey)
        servantError404
    Just blog -> do
        $logInfo $ "Read blog article from file: " <> T.pack (show blog)
        content <- readBlogEntry BlogFormatHTML blog
        $logInfo $ "Serve HTML blog article: " <> T.pack (show articleKey)
        pure $ toMarkup $ AsciiDocHtml content
