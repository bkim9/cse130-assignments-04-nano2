{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw4_nano (
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

bindir     = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\bin"
libdir     = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\lib\\x86_64-windows-ghc-8.10.7\\hw4-nano-0.1.0.0-ASTA9RB39sr5kaE3Arb6IQ"
dynlibdir  = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\share\\x86_64-windows-ghc-8.10.7\\hw4-nano-0.1.0.0"
libexecdir = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\libexec\\x86_64-windows-ghc-8.10.7\\hw4-nano-0.1.0.0"
sysconfdir = "C:\\Users\\beomsu_samsung\\Documents\\GitHub\\cse130-assignments-04-nano2\\.stack-work\\install\\5588a557\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw4_nano_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw4_nano_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw4_nano_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw4_nano_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw4_nano_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw4_nano_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
