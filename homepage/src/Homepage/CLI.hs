module Homepage.CLI where

import Homepage.Configuration

import GHC.Generics
import System.Console.GetOpt
import System.Environment

data Flag =
      DirectoryBlog FilePath
    | DirectoryFiles FilePath
    | DirectoryStatic FilePath
  deriving (Eq, Generic, Ord, Read, Show)

options :: [OptDescr Flag]
options = [ Option [] ["directory-blog"] (ReqArg DirectoryBlog "DIR") "Set blog directory"
          , Option [] ["directory-files"] (ReqArg DirectoryFiles "DIR") "Set files directory"
          , Option [] ["directory-static"] (ReqArg DirectoryStatic "DIR") "Set static directory"
          ]

controlOptions :: [Flag] -> Configuration
controlOptions [ DirectoryBlog configDirectoryBlog, DirectoryFiles configDirectoryFiles, DirectoryStatic configDirectoryStatic ] =
    Configuration { configDirectoryBlog, configDirectoryFiles, configDirectoryStatic }
controlOptions _ = error "Configure all options in order"

launch :: IO Configuration
launch = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (optArgs, [], []) -> pure $ controlOptions optArgs
    _ -> error "Configure exactly the correct options"
