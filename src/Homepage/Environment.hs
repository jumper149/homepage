module Homepage.Environment where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits

data Environment = Environment
    { envVarConfigFile :: FilePath
    , envVarLogFile :: Maybe FilePath
    }
  deriving stock (Eq, Generic, Ord, Read, Show)

class KnownSymbol envVar => EnvironmentVariable (envVar :: Symbol) where
  type EnvironmentVariableContent envVar
  parseEnvironmentVariable :: Proxy envVar -> String -> Maybe (EnvironmentVariableContent envVar)
  defaultEnvironmentVariable :: Proxy envVar -> EnvironmentVariableContent envVar
  askEnvironmentVariable :: Proxy envVar -> Environment -> EnvironmentVariableContent envVar

instance EnvironmentVariable "CONFIG_FILE" where
  type EnvironmentVariableContent "CONFIG_FILE" = FilePath
  parseEnvironmentVariable _ = Just
  defaultEnvironmentVariable _ = "./homepage.json"
  askEnvironmentVariable _ = envVarConfigFile

instance EnvironmentVariable "LOG_FILE" where
  type EnvironmentVariableContent "LOG_FILE" = Maybe FilePath
  parseEnvironmentVariable _ = Just . Just
  defaultEnvironmentVariable _ = Nothing
  askEnvironmentVariable _ = envVarLogFile
