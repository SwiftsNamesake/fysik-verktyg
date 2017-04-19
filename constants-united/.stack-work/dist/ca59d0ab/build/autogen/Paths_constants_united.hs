{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_constants_united (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\bin"
libdir     = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\lib\\x86_64-windows-ghc-8.0.2\\constants-united-0.1.0.0-Dsw9qjzcukv1YjvdfUjtl9"
dynlibdir  = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\share\\x86_64-windows-ghc-8.0.2\\constants-united-0.1.0.0"
libexecdir = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\libexec"
sysconfdir = "C:\\Users\\Jonatan\\SkyDrive\\Dokument\\Skola\\Jensen 2017\\verktyg\\constants-united\\.stack-work\\install\\05480a1e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "constants_united_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "constants_united_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "constants_united_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "constants_united_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "constants_united_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "constants_united_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
