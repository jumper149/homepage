{-# LANGUAGE TemplateHaskell #-}

module Homepage.Server.Route.Projects where

import Homepage.Application.Configured
import Homepage.Server.Html.Document
import Homepage.Configuration
import Homepage.Server.Tab

import Control.Monad.Logger
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

type API = Get '[HTML] Html

handler :: (MonadConfigured m, MonadLogger m)
        => ServerT API m
handler = do
  baseUrl <- configBaseUrl <$> configuration
  $logInfo "Serve projects overview."
  pure $ document baseUrl (Just 0) (Just TabProjects) $ do
    h2 "my Projects"
    ul $ do
      li $ do
        "A blue light filter in the style of XMonad: "
        a ! href "https://github.com/jumper149/blucontrol" $ "blucontrol"
        " replaces blugon for my personal usage"
      li $ do
        "The game "
        a ! href "https://github.com/jumper149/go" $ "go"
        " providing a smooth multiplayer browser experience with a webserver"
      li $ do
        "A simple Blue Light Filter for X: "
        a ! href "https://github.com/jumper149/blugon" $ "blugon"
      li $ do
        "Compiler from I- to S-Expression for the Scheme Programming Language: "
        a ! href "https://github.com/jumper149/haskeme" $ "haskeme"
      li $ do
        "The "
        a ! href "https://github.com/jumper149/dotfiles" $ "dotfiles"
        " to configure my ArchLinux-Systems"
