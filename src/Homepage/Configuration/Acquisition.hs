{-# LANGUAGE TemplateHaskell #-}

module Homepage.Configuration.Acquisition where

import Homepage.Configuration
import Homepage.Environment

import Control.Applicative (Const (..))
import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Posix.Files

acquireConfig :: (MonadIO m, MonadLogger m)
              => Environment
              -> m (Maybe Configuration)
acquireConfig Environment { envVarConfigFile }= do
  $logInfo "Checking configuration file."
  exists <- liftIO $ fileExist $ getConst envVarConfigFile
  if exists
     then do
       $logInfo "Reading configuration file."
       eitherContent <- liftIO $ A.eitherDecodeFileStrict $ getConst envVarConfigFile
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
