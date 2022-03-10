{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Homepage.Environment.Acquisition where

import Homepage.Environment

import Control.Applicative (Const (..))
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import System.Posix.Env

acquireEnvironment :: (MonadIO m, MonadLogger m)
                   => m Environment
acquireEnvironment = do
  $logInfo "Looking up environment variables."
  env <- liftIO getEnvironment
  $logDebug $ "Looked up environment variables: " <> T.pack (show env)

  envVarConfigFile <- lookupEnvironmentVariable @"HOMEPAGE_CONFIG_FILE" Proxy env
  envVarLogFile <- lookupEnvironmentVariable @"HOMEPAGE_LOG_FILE" Proxy env
  envVarLogLevel <- lookupEnvironmentVariable @"HOMEPAGE_LOG_LEVEL" Proxy env

  let environment = Environment {..}
  $logInfo $ "Processed environment variables and accumulated them: " <> T.pack (show environment)
  pure environment

lookupEnvironmentVariable :: (EnvironmentVariable envVar, MonadLogger m, Show (EnvironmentVariableContent envVar))
                          => Proxy envVar
                          -> [(String,String)]
                          -> m (Const (EnvironmentVariableContent envVar) envVar)
lookupEnvironmentVariable proxy env = do
  $logInfo $ "Inspecting environment variable: " <> T.pack (show envVarName)
  case lookup envVarName env of
    Nothing -> do
      $logInfo $ "Environment variable '" <> T.pack (show envVarName) <> "' is not set."
      $logInfo $ "Using default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
      pure $ Const envVarDefault
    Just str -> do
      $logInfo $ "Parsing environment variable: " <> T.pack (show envVarName)
      case parseEnvironmentVariable proxy str of
        Nothing -> do
          $logError $ "Failed to parse environment variable: " <> T.pack (show envVarName)
          $logWarn $ "Fall back to default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
          pure $ Const envVarDefault
        Just val -> do
          $logInfo $ "Parsed environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show val)
          pure $ Const val
  where
    envVarName = symbolVal proxy
    envVarDefault = defaultEnvironmentVariable proxy
