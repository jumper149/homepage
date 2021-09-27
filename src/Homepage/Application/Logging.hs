{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Logging where
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as B
import qualified Data.Time as T

newtype LoggingT' m a = LoggingT' { unLoggingT' :: LoggingT m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance MonadIO m => MonadLogger (LoggingT' m) where
  monadLoggerLog loc logSource logLevel logStr = do
    time <- lift $ liftIO T.getCurrentTime
    LoggingT' $ monadLoggerLog loc logSource logLevel $
      toLogStr $ B.pack (show time) <> " | " <> fromLogStr (toLogStr logStr)

runLoggingT' :: (MonadIO m, MonadBaseControl IO m)
             => Maybe FilePath
             -> LoggingT' m a
             -> m a
runLoggingT' Nothing = runStdoutLoggingT . unLoggingT'
runLoggingT' (Just filePath) = runFileLoggingT filePath . unLoggingT'

logDelayed :: MonadLogger m
           => LogLine
           -> m ()
logDelayed (loc, logSource, logLevel, logStr) = monadLoggerLog loc logSource logLevel logStr
