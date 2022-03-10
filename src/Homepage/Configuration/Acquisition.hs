{-# LANGUAGE TemplateHaskell #-}

module Homepage.Configuration.Acquisition where

import Homepage.Application.Environment.Class
import Homepage.Configuration

import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Posix.Files

acquireConfig :: (MonadIO m, MonadEnvironment m, MonadLogger m)
              => m (Maybe Configuration)
acquireConfig = do
  $logInfo "Checking configuration file."
  configFile <- environmentVariable $ EnvVar @"HOMEPAGE_CONFIG_FILE"
  exists <- liftIO $ fileExist configFile
  if exists
     then do
       $logInfo "Reading configuration file."
       eitherContent <- liftIO $ A.eitherDecodeFileStrict configFile
       case eitherContent of
         Left err -> do
           $logError $ "Failed to read/parse configuration file: " <> T.pack (show err)
           pure Nothing
         Right config -> do
           $logInfo $ "Acquired configuration: " <> T.pack (show config)
           pure $ Just config
     else do
       $logError "Can't find configuration file."
       pure Nothing
