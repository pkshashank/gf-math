{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gf_math (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/bin"
libdir     = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/lib/x86_64-linux-ghc-9.2.8/gf-math-0.1-EqW5GghZs5XBhdNGRWS6O8-Translate"
dynlibdir  = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/lib/x86_64-linux-ghc-9.2.8"
datadir    = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/share/x86_64-linux-ghc-9.2.8/gf-math-0.1"
libexecdir = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/libexec/x86_64-linux-ghc-9.2.8/gf-math-0.1"
sysconfdir = "/home/n05665sp/gf/gf-math/.stack-work/install/x86_64-linux-tinfo6/521afce3dac3341eca0eb4c6a899d85a69586821be5f2f201ee3331ce65d8302/9.2.8/etc"

getBinDir     = catchIO (getEnv "gf_math_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gf_math_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gf_math_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gf_math_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gf_math_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gf_math_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
