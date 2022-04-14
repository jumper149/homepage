module Homepage.Server.Route.Blog where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog
import Homepage.Server.Err404
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import Homepage.Server.Route.Blog.Atom qualified as Atom
import Homepage.Server.Tab

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Data.Text qualified as T
import Network.Wai.Trans
import Servant hiding (serveDirectoryWith)
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM.Server qualified as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Blaze.Html5.Extra
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

data Routes route = Routes
  { routeRaw :: route :- "raw" :> RawM.RawM
  , routeFeed :: route :- "atom.xml" :> Atom.API
  , routeArticle ::
      route
        :- Capture "article" BlogId
        :> UVerb 'GET '[HTML] '[WithStatus 200 Html, WithStatus 404 Html]
  , routeOverview :: route :- Get '[HTML] Html
  }
  deriving stock (Generic)

routes ::
  (MonadBaseControlIdentity IO m, MonadBlog m, MonadConfigured m, MonadLogger m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeRaw = rawHandler
    , routeFeed = Atom.handler
    , routeArticle = articleHandler
    , routeOverview = overviewHandler
    }

overviewHandler ::
  (MonadConfigured m, MonadLogger m) =>
  m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  blogs <- configBlogEntries <$> configuration
  logInfo "Serve blog overview."
  pure . document baseUrl contactInformation revision (Just 0) (Just TabBlog) $ do
    h2 "my Blog"
    p $ do
      "My blog is available as an "
      a ! hrefWithDepth baseUrl (Just 0) "blog/atom.xml" $ s "RSS" <> "/Atom Feed"
      "."
    blogList baseUrl (Just 0) blogs

articleHandler ::
  (MonadConfigured m, MonadLogger m) =>
  BlogId ->
  m (Union '[WithStatus 200 Html, WithStatus 404 Html])
articleHandler blogId = do
  blogs <- configBlogEntries <$> configuration
  case lookupBlog blogId blogs of
    Nothing -> do
      logWarn $ "Failed to serve blog article: " <> T.pack (show blogId)
      respond . WithStatus @404 =<< html404
    Just blog -> do
      baseUrl <- configBaseUrl <$> configuration
      contactInformation <- configContactInformation <$> configuration
      revision <- configRevision <$> configuration
      logInfo $ "Serve blog article: " <> T.pack (show blogId)
      respond . WithStatus @200 $
        document baseUrl contactInformation revision (Just 1) (Just TabBlog) $ do
          h2 $ text $ blogTitle blog
          p $ do
            "View blog entry: "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/raw/" <> unBlogId blogId <> ".html") $ "HTML"
            " | "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ "blog/raw/" <> unBlogId blogId <> ".pdf") $ "PDF"
          hr
          script ! HA.type_ "text/javascript" $
            "function resizeIframe(iframe) {\
            \  iframe.height = `${iframe.contentWindow.document.body.scrollHeight + 30}` + \"px\";\
            \}"
          iframe
            ! HA.src (withDepth baseUrl (Just 1) $ textValue $ "blog/raw/" <> unBlogId blogId <> ".html")
            ! HA.name "blog article (HTML)"
            ! HA.width "100%"
            ! HA.onload "resizeIframe(this)"
            ! HA.target "_parent"
            ! HA.style "border: none;"
            $ mempty

rawHandler ::
  (MonadBaseControlIdentity IO m, MonadConfigured m, MonadLogger m) =>
  ServerT RawM.RawM m
rawHandler = do
  directory <- configDirectoryBlog <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve blog download."
  RawM.serveDirectoryWith (defaultFileServerSettings directory) {ss404Handler = Just fallbackApplication}
