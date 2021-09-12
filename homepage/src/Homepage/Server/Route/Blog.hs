module Homepage.Server.Route.Blog where

import Homepage.Blog
import Homepage.Configuration
import Homepage.Server.Html.AsciiDoc
import Homepage.Server.Html.Blog
import Homepage.Server.Html.Document
import Homepage.Server.Tab

import Control.Monad.Base
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant hiding (serveDirectoryWith)
import Servant.HTML.Blaze
import Text.Blaze.Html5

type API = Get '[HTML] Html
      :<|> Capture "article" T.Text :>
        (    Get '[HTML] Html
        :<|> (    "adoc" :> Get '[HTML] Html
             :<|> "pdf" :> Get '[HTML] Html
             )
        )

handler :: (MonadBase IO m, MonadConfigured m) => ServerT API m
handler = overviewHandler
     :<|> articlesHandler

overviewHandler :: MonadConfigured m
                => m Html
overviewHandler = do
  blogs <- configBlogEntries <$> configuration
  pure $ document 0 (Just TabBlog) $ do
    h2 "my Blog"
    blogList blogs

articlesHandler :: (MonadBase IO m, MonadConfigured m)
                => T.Text
                -> (m Html :<|> m Html :<|> m Html)
articlesHandler article = htmlHandler article :<|> undefined :<|> undefined

htmlHandler :: (MonadBase IO m, MonadConfigured m)
            => T.Text
            -> m Html
htmlHandler articleKey = do
  blogs <- configBlogEntries <$> configuration
  dir <- configDirectoryBlog <$> configuration
  case lookupBlog articleKey blogs of
    Nothing -> undefined
    Just blog -> do
        content <- liftBase $ T.readFile $ dir <> "/" <> T.unpack (blogContent blog) <> ".html"
        pure $ document 1 (Just TabBlog) $
          toMarkup $ AsciiDocHtml content
