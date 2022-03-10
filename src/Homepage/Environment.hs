module Homepage.Environment where

import Control.Monad.Logger
import Control.Applicative (Const)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Text.Read

data Environment = Environment
    { envVarConfigFile :: Const FilePath "HOMEPAGE_CONFIG_FILE"
    , envVarLogFile :: Const (Maybe FilePath) "HOMEPAGE_LOG_FILE"
    , envVarLogLevel :: Const LogLevel "HOMEPAGE_LOG_LEVEL"
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

class KnownSymbol envVar => EnvironmentVariable (envVar :: Symbol) where
  type EnvironmentVariableContent envVar
  parseEnvironmentVariable :: Proxy envVar -> String -> Maybe (EnvironmentVariableContent envVar)
  defaultEnvironmentVariable :: Proxy envVar -> EnvironmentVariableContent envVar
  askEnvironmentVariable :: Proxy envVar -> Environment -> Const (EnvironmentVariableContent envVar) envVar

instance EnvironmentVariable "HOMEPAGE_CONFIG_FILE" where
  type EnvironmentVariableContent "HOMEPAGE_CONFIG_FILE" = FilePath
  parseEnvironmentVariable _ = Just
  defaultEnvironmentVariable _ = "./homepage.json"
  askEnvironmentVariable _ = envVarConfigFile

instance EnvironmentVariable "HOMEPAGE_LOG_FILE" where
  type EnvironmentVariableContent "HOMEPAGE_LOG_FILE" = Maybe FilePath
  parseEnvironmentVariable _ = Just . Just
  defaultEnvironmentVariable _ = Nothing
  askEnvironmentVariable _ = envVarLogFile

instance EnvironmentVariable "HOMEPAGE_LOG_LEVEL" where
  type EnvironmentVariableContent "HOMEPAGE_LOG_LEVEL" = LogLevel
  parseEnvironmentVariable _ = readMaybe
  defaultEnvironmentVariable _ = LevelInfo
  askEnvironmentVariable _ = envVarLogLevel
