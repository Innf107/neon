module Calcite.Config (Config (..), defaultConfig, getConfig, modifyConfig) where

import Calcite.Prelude

import System.IO.Unsafe

data Config = Config {
    printLir :: Bool
}

defaultConfig :: Config
defaultConfig = Config {
    printLir = False
}

configRef :: IORef Config
configRef = unsafePerformIO $ newIORef defaultConfig
{-# NOINLINE configRef #-}

getConfig :: () -> Config
getConfig () = unsafePerformIO $ readIORef configRef

modifyConfig :: (Config -> Config) -> IO ()
modifyConfig = modifyIORef' configRef

