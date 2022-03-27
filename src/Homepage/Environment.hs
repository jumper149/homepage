{-# LANGUAGE GADTs #-}

module Homepage.Environment where

import Control.Applicative
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

deriving stock instance Eq (EnvVarKind name value)
deriving stock instance Ord (EnvVarKind name value)
deriving stock instance Show (EnvVarKind name value)

class KnownSymbol name => KnownEnvVar (envVar :: EnvVarKind name value) | name -> envVar, envVar -> name, envVar -> value where
  parseEnvVar :: Proxy name -> String -> Maybe value
  defaultEnvVar :: Proxy name -> value
  caseEnvVar :: Proxy name -> EnvVarKind name value

newtype Environment = MkEnvironment { getEnvironment :: forall name value. EnvVarKind name value -> Const value name }
