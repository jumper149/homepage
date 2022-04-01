{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Logging where

import Homepage.Application.Environment.Class

import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Data.ByteString.Char8 qualified as B
import Data.Kind
import Data.Time qualified as T

newtype LoggingT' m a = LoggingT' { unLoggingT' :: LoggingT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance MonadIO m => MonadLogger (LoggingT' m) where
  monadLoggerLog loc logSource logLevel logStr = do
    time <- lift $ liftIO T.getCurrentTime
    LoggingT' $ monadLoggerLog loc logSource logLevel $
      toLogStr $ B.pack (show time) <> " | " <> fromLogStr (toLogStr logStr)

deriving via LoggingT' ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance MonadIO (t2 m) => MonadLogger (ComposeT LoggingT' t2 m)

runLoggingT' :: (MonadIO m, MonadBaseControl IO m)
             => Maybe FilePath
             -> LogLevel
             -> LoggingT' m a
             -> m a
runLoggingT' maybeFilePath configuredLogLevel = run . withFilter . unLoggingT'
  where
    run = maybe runStdoutLoggingT runFileLoggingT maybeFilePath
    withFilter = filterLogger $ \ _src lvl -> lvl >= configuredLogLevel

runAppLoggingT' :: (MonadBaseControl IO m, MonadEnvironment m, MonadIO m) => LoggingT' m a -> m a
runAppLoggingT' tma = do
  maybeLogFile <- environmentVariable $ EnvVar @"HOMEPAGE_LOG_FILE"
  logLevel <- environmentVariable $ EnvVar @"HOMEPAGE_LOG_LEVEL"
  runLoggingT' maybeLogFile logLevel tma

logLine :: MonadLogger m
        => LogLine
        -> m ()
logLine (loc, logSource, logLevel, logStr) = monadLoggerLog loc logSource logLevel logStr
