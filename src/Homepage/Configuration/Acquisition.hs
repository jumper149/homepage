{-# LANGUAGE TemplateHaskell #-}

module Homepage.Configuration.Acquisition where

import Homepage.Configuration

import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Environment
import System.Posix.Files

configFileDefault :: FilePath
configFileDefault = "./homepage.json"

configFileEnvironmentVariable :: String
configFileEnvironmentVariable = "HOMEPAGE_CONFIG_FILE"

acquireConfig :: (MonadIO m, MonadLogger m)
              => m (Maybe Configuration)
acquireConfig = do
  $logInfo $ "Lookup environment variable: " <> T.pack (show configFileEnvironmentVariable)
  maybeFilePath <- liftIO $ lookupEnv configFileEnvironmentVariable
  filePath <- case maybeFilePath of
    Nothing -> do
      $logInfo $ "Using default configuration file: " <> T.pack (show configFileDefault)
      pure configFileDefault
    Just fp -> do
      $logInfo $ "Using configuration file: " <> T.pack (show fp)
      pure fp
  $logInfo "Checking configuration file."
  exists <- liftIO $ fileExist filePath
  if exists
     then do
       $logInfo "Reading configuration file."
       eitherContent <- liftIO $ A.eitherDecodeFileStrict filePath
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
