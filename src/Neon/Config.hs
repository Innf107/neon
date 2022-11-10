module Neon.Config (Config (..), defaultConfig, getConfig, modifyConfig) where

import Neon.Prelude

import System.IO.Unsafe

data Config = Config {
    printTokens :: Bool
,   printMir :: Bool
,   printMc :: Bool
,   printLocalPrefix :: Bool
}

defaultConfig :: Config
defaultConfig = Config {
    printTokens = False
,   printMir = False
,   printMc = False
,   printLocalPrefix = False
}

configRef :: IORef Config
configRef = unsafePerformIO $ newIORef defaultConfig
{-# NOINLINE configRef #-}

getConfig :: () -> Config
getConfig () = unsafePerformIO $ readIORef configRef

modifyConfig :: (Config -> Config) -> IO ()
modifyConfig = modifyIORef' configRef

