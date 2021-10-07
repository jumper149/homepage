{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Files where

import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Tab
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import Homepage.Server.Html.Files
import Homepage.Server.Err404

import Control.Monad.Logger
import Servant
import Servant.HTML.Blaze
import qualified Servant.RawM.Server as RawM
import Text.Blaze.Html5
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = Get '[HTML] Html
      :<|> RawM.RawM

handler :: (MonadConfigured m, MonadLogger m)
        => ServerT API m
handler = overviewHandler :<|> filesHandler

overviewHandler :: (MonadConfigured m, MonadLogger m)
                => m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  fileEntries <- configFileEntries <$> configuration
  $logInfo "Serve files overview."
  pure $ document baseUrl (Just 0) (Just TabFiles) $ do
    h2 "my Files"
    fileList baseUrl (Just 0) fileEntries

filesHandler :: (MonadConfigured m, MonadLogger m)
             => ServerT RawM.RawM m
filesHandler = do
  directory <- configDirectoryFiles <$> configuration
  fallbackApplication <- application404
  $logInfo "Serve file download."
  RawM.serveDirectoryWith (defaultFileServerSettings directory) { ss404Handler = Just fallbackApplication }
