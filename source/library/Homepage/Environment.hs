{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Homepage.Environment where

import Control.Monad.Logger
import Data.Kind
import Data.Singletons.TH
import GHC.Generics
import GHC.TypeLits
import Text.Read

data EnvVar
  = EnvVarConfigFile
  | EnvVarLogFile
  | EnvVarLogLevel
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

$(genSingletons [''EnvVar])

type family EnvVarName (envVar :: EnvVar) = (name :: Symbol) | name -> envVar where
  EnvVarName 'EnvVarConfigFile = "HOMEPAGE_CONFIG_FILE"
  EnvVarName 'EnvVarLogFile = "HOMEPAGE_LOG_FILE"
  EnvVarName 'EnvVarLogLevel = "HOMEPAGE_LOG_LEVEL"

type family EnvVarValue (envVar :: EnvVar) = (value :: Type) where
  EnvVarValue 'EnvVarConfigFile = FilePath
  EnvVarValue 'EnvVarLogFile = Maybe FilePath
  EnvVarValue 'EnvVarLogLevel = LogLevel

parseEnvVar :: Sing envVar -> String -> Maybe (EnvVarValue envVar)
parseEnvVar = \case
  SEnvVarConfigFile -> Just
  SEnvVarLogFile -> Just . Just
  SEnvVarLogLevel -> readMaybe

defaultEnvVar :: Sing envVar -> EnvVarValue envVar
defaultEnvVar = \case
  SEnvVarConfigFile -> "./homepage.json"
  SEnvVarLogFile -> Nothing
  SEnvVarLogLevel -> LevelDebug

newtype Environment = MkEnvironment {getEnvironment :: forall envVar. Sing envVar -> EnvVarValue envVar}
