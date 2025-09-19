{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_glados (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/bin"
libdir     = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/lib/x86_64-linux-ghc-9.6.3/glados-0.1.0.0-CKDoUNbafJmLVUDugBDXO3-glc"
dynlibdir  = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/lib/x86_64-linux-ghc-9.6.3"
datadir    = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/share/x86_64-linux-ghc-9.6.3/glados-0.1.0.0"
libexecdir = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/libexec/x86_64-linux-ghc-9.6.3/glados-0.1.0.0"
sysconfdir = "/home/prestige/delivery/Semester5/B-FUN/G-FUN-500-TLS-5-1-glados-2/.stack-work/install/x86_64-linux-tinfo6/d7d410d3f8dd7e3ae1425c9aa38cadefaaf48152d5936ff3de578d02409ce02c/9.6.3/etc"

getBinDir     = catchIO (getEnv "glados_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "glados_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "glados_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "glados_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glados_sysconfdir") (\_ -> return sysconfdir)



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
