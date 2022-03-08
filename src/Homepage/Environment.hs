module Homepage.Environment where

import GHC.Generics

data Environment = Environment
    { envVarConfigFile :: FilePath
    , envVarLogFile :: Maybe FilePath
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
