module Paths_Fractals (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/warrick/.cabal/bin"
libdir     = "/home/warrick/.cabal/lib/x86_64-linux-ghc-7.10.3/Fractals-1.0-9ZnC4WoyeDq64mz8yU1fer"
datadir    = "/home/warrick/.cabal/share/x86_64-linux-ghc-7.10.3/Fractals-1.0"
libexecdir = "/home/warrick/.cabal/libexec"
sysconfdir = "/home/warrick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Fractals_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Fractals_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Fractals_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Fractals_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Fractals_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
