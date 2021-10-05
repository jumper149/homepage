{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Files where

import Homepage.Application.Configured
import Homepage.Configuration
import Homepage.Server.Tab
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document
import Homepage.Server.Err404

import Control.Monad.Logger
import Servant
import Servant.HTML.Blaze
import qualified Servant.RawM.Server as RawM
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
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
  $logInfo "Serve files overview."
  pure $ document baseUrl (Just 0) (Just TabFiles) $ do
    h2 "my Files"
    ul $ do
      li $ do
        "curriculum vitae (24/06/21) "
        H.span ! class_ "options" $ do
            "[ "
            a ! hrefWithDepth baseUrl (Just 0) "files/Felix_Springer-cv.pdf" $ "PDF"
            " ]"
      li $ do
        "mail signing key (02/09/20) "
        H.span ! class_ "options" $ do
            "[ "
            a ! hrefWithDepth baseUrl (Just 0) "files/Felix_Springer-publickey-mail.gpg" $ "GPG"
            " ]"
    h2 "my Publications"
    ul $ do
      li $ do
        "Storage Register Design for an Ion Trap Quantum Processor (07/04/21) "
        H.span ! class_ "options" $ do
          "[ "
          a ! hrefWithDepth baseUrl (Just 0) "files/Felix_Springer-Storage_Register_Design_for_an_Ion_Trap_Quantum_Processor.pdf" $ "PDF"
          " ]"


filesHandler :: (MonadConfigured m, MonadLogger m)
             => ServerT RawM.RawM m
filesHandler = do
  directory <- configDirectoryFiles <$> configuration
  fallbackApplication <- application404
  $logInfo "Serve file download."
  RawM.serveDirectoryWith (defaultFileServerSettings directory) { ss404Handler = Just fallbackApplication }
