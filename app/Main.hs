module Main where

import Neon.Prelude

import Neon.Driver
import Neon.Packager

import System.Environment (getArgs)

import Data.Text qualified as Text
import Neon.Config (modifyConfig, Config (..), getConfig)

import Codec.Archive.Zip (fromArchive)

usage :: Text
usage = unlines [
        "usage: neonc [OPTIONS] <FILE>"
    ,   ""
    ,   "OPTIONS"
    ,   "--print-mir            Print the mid level IR for debugging purposes"
    ,   "--print-mc             Print the generated minecraft functions for debugging purposes"
    ,   "--print-local-prefix   Include a prefix consisting of module and function name in locals"
    ]
failUsage :: Text -> IO a
failUsage message = putTextLn (message <> "\n\n" <> usage) >> exitFailure

prettyLowerWarning :: LowerWarning -> Text
prettyLowerWarning = \case
    UnreachableCode reason -> "\ESC[1m\ESC[95m\STXWARNING:\ESC[0m\ESC[1m\STX Unreachable code. Reason: " <> show reason <> "\ESC[0m\STX"

main :: IO ()
main = do    
    file <- parseArgs >>= \case
        [file] -> pure file
        [] -> failUsage "Missing required argument"
        _ -> failUsage "Too many arguments"
    content <- readFileText (toString file)
    let name = fromMaybe file $ Text.stripSuffix ".neon" file
    result <- runM $ runError $ runOutputStdout prettyLowerWarning $ compileToMC name content
    case result of
        Left e          -> print e
        Right mcfuns    -> do
            let Config { printMc } = getConfig ()
            when printMc $ mapM_ printFun mcfuns
            let archive = datapackToZip name mcfuns
            writeFileLBS (toString name <> ".zip") (fromArchive archive)
                where
                    printFun :: (FilePath, Text) -> IO ()
                    printFun (name, commands) = do
                        putStrLn name
                        putTextLn commands
                        putTextLn ""

parseArgs :: IO [Text]
parseArgs = getArgs >>= go
    where
        go [] = pure []
        go ("--print-mir" : args) = do
            modifyConfig (\config -> config{printLir = True})
            go args
        go ("--print-mc" : args) = do
            modifyConfig (\config -> config{printMc = True})
            go args
        go ("--print-local-prefix" : args) = do
            modifyConfig (\config -> config{printLocalPrefix = True})
            go args
        go ((toText -> arg) : args)
            | "-" `Text.isPrefixOf` arg = failUsage $ "Invalid flag '" <> arg <> "'"
            | otherwise = (arg :) <$> go args
