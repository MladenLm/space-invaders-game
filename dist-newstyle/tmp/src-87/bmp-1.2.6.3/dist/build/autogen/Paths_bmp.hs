{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_bmp (
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
version = Version [1,2,6,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/bin"
libdir     = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/lib"
dynlibdir  = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/lib"
datadir    = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/share"
libexecdir = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/libexec"
sysconfdir = "/home/runner/.cabal/store/ghc-8.10.6/bmp-1.2.6.3-222850c34917429332f5db37d02bb5af20901bb33b6cd02ea81adee5f71a9e1c/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bmp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bmp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bmp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bmp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bmp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bmp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
