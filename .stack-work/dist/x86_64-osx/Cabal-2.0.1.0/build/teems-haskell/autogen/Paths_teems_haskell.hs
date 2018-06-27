{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_teems_haskell (
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

bindir     = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/bin"
libdir     = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/lib/x86_64-osx-ghc-8.2.2/teems-haskell-0.1.0.0-43MWoOXZItp4K9iZRZ5nt2-teems-haskell"
dynlibdir  = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/share/x86_64-osx-ghc-8.2.2/teems-haskell-0.1.0.0"
libexecdir = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/libexec/x86_64-osx-ghc-8.2.2/teems-haskell-0.1.0.0"
sysconfdir = "/Users/yuuki/private/teems-haskell/.stack-work/install/x86_64-osx/lts-11.13/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "teems_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "teems_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "teems_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "teems_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "teems_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "teems_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
