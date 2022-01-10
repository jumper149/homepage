{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Blog where

import Homepage.Application.Blog
import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Configuration.Blog
import Homepage.Server.Err404
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import qualified Homepage.Server.Route.Blog.Atom as Atom
import Homepage.Server.Tab

import Control.Monad.Error.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Servant hiding (serveDirectoryWith)
import Servant.API.Generic
import Servant.HTML.Blaze
import qualified Servant.RawM.Server as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import qualified Text.Blaze.Html5.Attributes as H
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

data Routes route = Routes
    { routeRaw :: route
               :- "raw"
               :> RawM.RawM
    , routeFeed :: route
                :- "atom.xml"
                :> Atom.API
    , routeArticle :: route
                   :- Capture "article" BlogId
                   :> Get '[HTML] Html
    , routeOverview :: route
                    :- Get '[HTML] Html
    }
  deriving Generic

routes :: (MonadBlog m, MonadConfigured m, MonadError ServerError m, MonadLogger m)
       => Routes (AsServerT m)
routes = Routes
    { routeRaw = rawHandler
    , routeFeed = Atom.handler
    , routeArticle = articleHandler
    , routeOverview = overviewHandler
    }

overviewHandler :: (MonadConfigured m, MonadLogger m)
                => m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  blogs <- configBlogEntries <$> configuration
  $logInfo "Serve blog overview."
  pure $ document baseUrl contactInformation (Just 0) (Just TabBlog) $ do
    h2 "my Blog"
    p $ do
      "My blog is available as an "
      a ! hrefWithDepth baseUrl (Just 0) "blog/atom.xml" $ s "RSS" <> "/Atom Feed"
      "."
    blogList baseUrl (Just 0) blogs

articleHandler :: (MonadConfigured m, MonadError ServerError m, MonadLogger m)
               => BlogId
               -> m Html
articleHandler blogId = do
  baseUrl <- configBaseUrl <$> configuration
  blogs <- configBlogEntries <$> configuration
  case lookupBlog blogId blogs of
    Nothing -> do
        $logWarn $ "Failed to serve blog article: " <> T.pack (show blogId)
        servantError404
    Just blog -> do
        contactInformation <- configContactInformation <$> configuration
        $logInfo $ "Serve blog article: " <> T.pack (show blogId)
        pure $ document baseUrl contactInformation (Just 1) (Just TabBlog) $ do
          h2 $ text $ blogTitle blog
          p $ do
            "View blog entry: "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/raw/" <> unBlogId blogId <> ".html") $ "HTML"
            " | "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/raw/" <> unBlogId blogId <> ".pdf") $ "PDF"
          hr
          script ! H.type_ "text/javascript" $
            "function resizeIframe(iframe) {\
            \  iframe.height = `${iframe.contentWindow.document.body.scrollHeight + 30}` + \"px\";\
            \}"
          iframe ! H.src (withDepth baseUrl (Just 1) $ textValue $ "blog/raw/" <> unBlogId blogId <> ".html")
                 ! H.name "blog article (HTML)"
                 ! H.width "100%"
                 ! H.onload "resizeIframe(this)"
                 ! H.target "_parent"
                 ! H.style "border: none;"
                 $ mempty

rawHandler :: (MonadConfigured m, MonadLogger m)
           => ServerT RawM.RawM m
rawHandler = do
  directory <- configDirectoryBlog <$> configuration
  fallbackApplication <- application404
  $logInfo "Serve blog download."
  RawM.serveDirectoryWith (defaultFileServerSettings directory) { ss404Handler = Just fallbackApplication }
