{-# LANGUAGE TemplateHaskell #-}

module Homepage.Configuration.Acquisition where

import Homepage.Configuration

import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Posix.Env
import System.Posix.Files

configFileEnvironmentVariable :: String
configFileEnvironmentVariable = "HOMEPAGE_CONFIG_FILE"

logFileEnvironmentVariable :: String
logFileEnvironmentVariable = "HOMEPAGE_LOG_FILE"

acquirePreConfig :: (MonadIO m, MonadLogger m)
                 => m PreConfiguration
acquirePreConfig = do

  $logInfo $ "Looking up environment variable: " <> T.pack (show configFileEnvironmentVariable)
  maybeConfigFilePath <- liftIO $ getEnv configFileEnvironmentVariable
  preConfigConfigFile <- case maybeConfigFilePath of
    Nothing -> do
      let configFileDefault = "./homepage.json"
      $logWarn $ "Using default configuration file: " <> T.pack (show configFileDefault)
      pure configFileDefault
    Just fp -> do
      $logInfo $ "Using configuration file: " <> T.pack (show fp)
      pure fp

  $logInfo $ "Looking up environment variable: " <> T.pack (show logFileEnvironmentVariable)
  preConfigLogFile <- liftIO $ getEnv logFileEnvironmentVariable
  case preConfigLogFile of
    Nothing -> $logInfo "Using no log file."
    Just fp -> $logInfo $ "Using log file: " <> T.pack (show fp)

  pure PreConfiguration { preConfigConfigFile, preConfigLogFile }

acquireConfig :: (MonadIO m, MonadLogger m)
              => PreConfiguration
              -> m (Maybe Configuration)
acquireConfig PreConfiguration { preConfigConfigFile }= do
  $logInfo "Checking configuration file."
  exists <- liftIO $ fileExist preConfigConfigFile
  if exists
     then do
       $logInfo "Reading configuration file."
       eitherContent <- liftIO $ A.eitherDecodeFileStrict preConfigConfigFile
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
