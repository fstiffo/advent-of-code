module Paths_advent_of_code (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Users\\Francesco\\HaskellProjects\\advent-of-code\\.stack-work\\install\\462e5c81\\bin"
libdir     = "D:\\Users\\Francesco\\HaskellProjects\\advent-of-code\\.stack-work\\install\\462e5c81\\lib\\x86_64-windows-ghc-7.10.3\\advent-of-code-0.1.0.0-FIPrw1MtQ0T9c9bMhTKjnZ"
datadir    = "D:\\Users\\Francesco\\HaskellProjects\\advent-of-code\\.stack-work\\install\\462e5c81\\share\\x86_64-windows-ghc-7.10.3\\advent-of-code-0.1.0.0"
libexecdir = "D:\\Users\\Francesco\\HaskellProjects\\advent-of-code\\.stack-work\\install\\462e5c81\\libexec"
sysconfdir = "D:\\Users\\Francesco\\HaskellProjects\\advent-of-code\\.stack-work\\install\\462e5c81\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "advent_of_code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "advent_of_code_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "advent_of_code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "advent_of_code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "advent_of_code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
