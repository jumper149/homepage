module Homepage.Server.FileServer where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Data.Text qualified as T
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

fileServerSettings ::
  (MonadBaseControlIdentity IO m, MonadLogger m) =>
  FilePath ->
  m StaticSettings
fileServerSettings path =
  liftBaseWithIdentity $ \runInIO ->
    pure $ case defaultFileServerSettings path of
      defaultSettings@StaticSettings {ssLookupFile, ssGetMimeType} ->
        defaultSettings
          { ssLookupFile = \pieces -> do
              runInIO . logInfo $ "Looking up file: " <> T.pack (show pieces)
              ssLookupFile pieces
          , ssGetMimeType = \file -> do
              mimeType <- ssGetMimeType file
              runInIO . logInfo $ "Determined mime type: " <> T.pack (show mimeType)
              pure mimeType
          }
