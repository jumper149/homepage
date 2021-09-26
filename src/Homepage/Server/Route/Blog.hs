{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Blog where

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

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant hiding (serveDirectoryWith)
import Servant.HTML.Blaze
import Text.Blaze.Html5

type API = "atom.xml" :> Atom.API
      :<|> Capture "article" T.Text :>
        (    Get '[HTML] Html
        :<|> (    "adoc" :> Get '[HTML] Html
             :<|> "pdf" :> Get '[HTML] Html
             )
        )
      :<|> Get '[HTML] Html

handler :: (MonadBase IO m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
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

articlesHandler :: (MonadBase IO m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
                => T.Text
                -> (m Html :<|> m Html :<|> m Html)
articlesHandler articleKey = htmlHandler articleKey :<|> undefined :<|> undefined

htmlHandler :: (MonadBase IO m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
            => T.Text
            -> m Html
htmlHandler articleKey = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  dir <- configDirectoryBlog <$> configuration
  case lookupBlog articleKey blogs of
    Nothing -> do
        $logError $ "Failed to serve blog article: " <> T.pack (show articleKey)
        servantError404
    Just blog -> do
        let file = dir <> "/" <> T.unpack (blogContent blog) <> ".html"
        $logInfo $ "Read blog article from file: " <> T.pack (show file)
        content <- liftBase $ T.readFile file
        $logInfo $ "Serve HTML blog article: " <> T.pack (show articleKey)
        pure $ document baseUrl (Just 1) (Just TabBlog) $
          toMarkup $ AsciiDocHtml content
