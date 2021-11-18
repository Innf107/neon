module Main where

import Calcite.Prelude

import Calcite.Driver
import Calcite.Packager
import Language.McFunction.Types (CompiledModule)
import Language.McFunction.PrettyPrint

main :: IO ()
main = do
    content <- readFile "test.cal"
    let result = run $ runError $ compileToCodegen $ toText content
    case result of
        Left e      -> print e
        Right ms    -> do 
            mapM_ printMod ms
            writeFileLBS "test.zip" (fromArchive (datapackToZip (package "test" ms)))
    where
        printMod :: CompiledModule -> IO ()
        printMod (name, commands) = let ?namespace = "test" in do
            putStrLn name
            mapM_ (putTextLn . ("  "<>) . prettyCommand) commands

