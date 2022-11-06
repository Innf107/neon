module Main where

import Calcite.Prelude

import Calcite.Driver
import Calcite.Packager
import Language.McFunction.Types (CompiledModule)
import Language.McFunction.PrettyPrint

import System.Environment (getArgs)

import Data.Text qualified as Text
import Calcite.Config (modifyConfig, Config (..))

usage :: Text
usage = unlines [
        "usage: calc [OPTIONS] <FILE>"
    ,   ""
    ,   "OPTIONS"
    ,   "--print-mir          Print the mid level IR for debugging purposes"
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
    result <- runM $ runError $ runOutputStdout prettyLowerWarning $ compileToMC (fromMaybe file $ Text.stripSuffix ".cal" file) content
    case result of
        Left e          -> print e
        Right mcmods    -> do 
            mapM_ printMod mcmods 
            -- TODO: We cannot fully compile to a datapack right now
    where
        printMod :: (FilePath, Text) -> IO ()
        printMod (name, commands) = do
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
        go ((toText -> arg) : args)
            | "-" `Text.isPrefixOf` arg = failUsage $ "Invalid flag '" <> arg <> "'"
            | otherwise = (arg :) <$> go args
