{-# LANGUAGE TemplateHaskell #-}

module Homepage.Application.Environment.Acquisition where

import Homepage.Environment
import Homepage.Application.Environment

import Control.Applicative
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text qualified as T
import GHC.TypeLits
import System.Posix.Env qualified as System

acquireEnvironment :: (MonadIO m, MonadLogger m)
                   => m Environment
acquireEnvironment = do
  $logInfo "Looking up environment variables."
  env <- liftIO System.getEnvironment
  $logDebug $ "Looked up environment variables: " <> T.pack (show env)

  configFile <- lookupEnvironmentVariable env $ Proxy @"HOMEPAGE_CONFIG_FILE"
  logFile <- lookupEnvironmentVariable env $ Proxy @"HOMEPAGE_LOG_FILE"
  logLevel <- lookupEnvironmentVariable env $ Proxy @"HOMEPAGE_LOG_LEVEL"

  let environment = MkEnvironment $ \case
        EnvVarConfigFile -> configFile
        EnvVarLogFile -> logFile
        EnvVarLogLevel -> logLevel
  pure environment

lookupEnvironmentVariable :: forall name val (envVar :: EnvVarKind name val) m. (KnownEnvVar envVar, MonadLogger m, Show val)
                          => [(String,String)]
                          -> Proxy name
                          -> m (Const val name)
lookupEnvironmentVariable env proxy = do
  $logInfo $ "Inspecting environment variable: " <> T.pack (show envVarName)
  case lookup envVarName env of
    Nothing -> do
      $logInfo $ "Environment variable '" <> T.pack (show envVarName) <> "' is not set."
      $logInfo $ "Using default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
      pure $ Const envVarDefault
    Just str -> do
      $logInfo $ "Parsing environment variable: " <> T.pack (show envVarName)
      case parseEnvVar proxy str of
        Nothing -> do
          $logError $ "Failed to parse environment variable: " <> T.pack (show envVarName)
          $logWarn $ "Fall back to default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
          pure $ Const envVarDefault
        Just val -> do
          $logInfo $ "Parsed environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show val)
          pure $ Const val
  where
    envVarName = symbolVal proxy
    envVarDefault = defaultEnvVar proxy
