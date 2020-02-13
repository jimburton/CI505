{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_huffman_exercise (
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

bindir     = "/home/jb259/.cabal/bin"
libdir     = "/home/jb259/.cabal/lib/x86_64-linux-ghc-8.0.2/huffman-exercise-0.1.0.0-inplace-huffman-tests"
dynlibdir  = "/home/jb259/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/jb259/.cabal/share/x86_64-linux-ghc-8.0.2/huffman-exercise-0.1.0.0"
libexecdir = "/home/jb259/.cabal/libexec/x86_64-linux-ghc-8.0.2/huffman-exercise-0.1.0.0"
sysconfdir = "/home/jb259/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "huffman_exercise_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "huffman_exercise_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "huffman_exercise_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "huffman_exercise_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "huffman_exercise_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "huffman_exercise_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
