{-# LANGUAGE GADTs #-}

module Homepage.Application.Environment.Acquisition where

import Homepage.Environment

import Control.Applicative
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.IO.Class
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.State
import Data.List (deleteBy, isPrefixOf)
import Data.Proxy
import Data.Text qualified as T
import GHC.TypeLits
import System.Posix.Env qualified as System

acquireEnvironment :: (MonadIO m, MonadLogger m)
                   => m Environment
acquireEnvironment = do
  logInfo "Looking up environment variables."
  env <- liftIO System.getEnvironment
  logDebug $ "Looked up environment variables: " <> T.pack (show env)

  (environment, unconsumedEnv) <- flip runStateT env . descend $ do
    configFile <- lookupEnvironmentVariable $ Proxy @"HOMEPAGE_CONFIG_FILE"
    logFile <- lookupEnvironmentVariable $ Proxy @"HOMEPAGE_LOG_FILE"
    logLevel <- lookupEnvironmentVariable $ Proxy @"HOMEPAGE_LOG_LEVEL"

    let environment = MkEnvironment $ \case
          EnvVarConfigFile -> configFile
          EnvVarLogFile -> logFile
          EnvVarLogLevel -> logLevel
    pure environment

  checkConsumedEnvironment unconsumedEnv
  pure environment

lookupEnvironmentVariable :: forall name value (envVar :: EnvVarKind name value) m.
                             (KnownEnvVar envVar, MonadLogger m, Show value)
                          => Proxy name
                          -> Elevator (StateT [(String,String)]) m (Const value name)
lookupEnvironmentVariable proxy = do
  logInfo $ "Inspecting environment variable: " <> T.pack (show envVarName)
  env <- Ascend get
  case lookup envVarName env of
    Nothing -> do
      logInfo $ "Environment variable '" <> T.pack (show envVarName) <> "' is not set."
      logInfo $ "Using default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
      pure $ Const envVarDefault
    Just str -> do
      logInfo $ "Spotted environment variable: " <> T.pack (show envVarName)
      Ascend $ modify $ deleteBy (\ x y -> fst x == fst y) (envVarName, undefined)
      logDebug $ "Parsing environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show str)
      case parseEnvVar proxy str of
        Nothing -> do
          logError $ "Failed to parse environment variable: " <> T.pack (show envVarName)
          logWarn $ "Falling back to default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefault)
          pure $ Const envVarDefault
        Just val -> do
          logInfo $ "Parsed environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show val)
          pure $ Const val
  where
    envVarName = symbolVal proxy
    envVarDefault = defaultEnvVar proxy

checkConsumedEnvironment :: MonadLogger m
                         => [(String,String)]
                         -> m ()
checkConsumedEnvironment env = do
  logDebug $ "Check unconsumed environment for left-over environment variables: " <> T.pack (show env)
  case filter isSuspicious env of
    [] -> logInfo "Unconsumed environment doesn't contain any anomalies."
    anomalies -> logWarn $ "Unconsumed environment contains anomalies: " <> T.pack (show anomalies)
  where
    isSuspicious :: (String,String) -> Bool
    isSuspicious (identifier, _value) = "HOMEPAGE" `isPrefixOf` identifier
