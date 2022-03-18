{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Homepage.Environment where

import Control.Monad.Logger
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Text.Read

data EnvVarKind :: Symbol -> Type -> Type where
  EnvVarConfigFile :: EnvVarKind "HOMEPAGE_CONFIG_FILE" FilePath
  EnvVarLogFile :: EnvVarKind "HOMEPAGE_LOG_FILE" (Maybe FilePath)
  EnvVarLogLevel :: EnvVarKind "HOMEPAGE_LOG_LEVEL" LogLevel

instance KnownEnvVar 'EnvVarConfigFile where
  parseEnvVar _ = Just
  defaultEnvVar _ = "./homepage.json"
  caseEnvVar _ = EnvVarConfigFile

instance KnownEnvVar 'EnvVarLogFile where
  parseEnvVar _ = Just . Just
  defaultEnvVar _ = Nothing
  caseEnvVar _ = EnvVarLogFile

instance KnownEnvVar 'EnvVarLogLevel where
  parseEnvVar _ = readMaybe
  defaultEnvVar _ = LevelDebug
  caseEnvVar _ = EnvVarLogLevel

class KnownSymbol name => KnownEnvVar (envVar :: EnvVarKind name value) | name -> envVar, envVar -> name, envVar -> value where
  parseEnvVar :: Proxy name -> String -> Maybe value
  defaultEnvVar :: Proxy name -> value
  caseEnvVar :: Proxy name -> EnvVarKind name value
