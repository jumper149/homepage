module Homepage.Server.Route.Blog where

import Homepage.Configuration
import Homepage.Server.Err404
import Homepage.Server.Html.Document
import Homepage.Server.Tab

import Servant hiding (serveDirectoryWith)
import Servant.HTML.Blaze
import Servant.RawM.Server
import Text.Blaze.Html5
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = Get '[HTML] Html :<|> RawM

handler :: MonadConfigured m => ServerT API m
handler = overviewHandler
     :<|> articlesHandler

overviewHandler :: Monad m
                => m Html
overviewHandler = pure $
  document (Just TabBlog) $
    h2 "my Blog"

articlesHandler :: MonadConfigured m
                => ServerT RawM m
articlesHandler = do
    path <- configDirectoryBlog <$> configuration
    serveDirectoryWith (defaultFileServerSettings path) { ss404Handler = Just application404 }
