module Neon.Driver ( 
      CompilerError(..)
    , LowerWarning (..)
    , compileToMC
    ) where

import Neon.Prelude
import Neon.Syntax
import Neon.Pretty

import Neon.Lexer
import Neon.Parser qualified as Parser
import Neon.Rename
import Neon.Typecheck
import Neon.Packager

import Neon.MIR
import Neon.NeonToMIR as NeonToMIR
import Neon.MIRToMC as MIRToMC

import Neon.Config

data CompilerError = LexicalError LexError
                   | RenameError RenameError
                   | TypeError TypeError
                   deriving Show    

compileToMC :: Members '[Error CompilerError, Embed IO, Output LowerWarning] r => Text -> Text -> Sem r [(FilePath, Text)]
compileToMC name code = do
    let Config { printTokens, printMir } = getConfig ()

    -- TODO: Include the file name instead of the module name
    tokens <- mapError LexicalError $ lex (toString name) code
    when printTokens $ mapM_ (print . tokenClass) tokens

    let syntax = Parser.parse tokens
    renamed <- mapError RenameError $ rename (emptyModuleEnv name) syntax
    typed <- mapError TypeError $ evalState (TCState mempty) $ typecheck renamed

    mir <- NeonToMIR.compile typed
    when printMir $ putTextLn $ pretty mir

    MIRToMC.compile mir 

        