{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_xmonad_config (
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

bindir     = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/bin"
libdir     = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/lib/x86_64-linux-ghc-8.6.5/xmonad-config-0.1.0.0-FpNSPiL5Kp266J274SBmva-xmonad"
dynlibdir  = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/share/x86_64-linux-ghc-8.6.5/xmonad-config-0.1.0.0"
libexecdir = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/libexec/x86_64-linux-ghc-8.6.5/xmonad-config-0.1.0.0"
sysconfdir = "/home/kiyengar/dotfiles/xmonad/.xmonad/.stack-work/install/x86_64-linux-tinfo6/28751cc65dd5227ae4f92dd1cd5f02c5a8577381639ef9891b58f5af0c94e7c4/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_config_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmonad_config_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmonad_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_config_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
