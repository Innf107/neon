module Neon.Driver ( 
      CompilerError(..)
    , LowerWarning (..)
    , compileToMC
    ) where

import Neon.Prelude
import Neon.Syntax
import Neon.Pretty

import Neon.Lexer
import Neon.Parser
import Neon.Rename
import Neon.Typecheck
import Neon.Packager

import Neon.MIR
import Neon.NeonToMIR as NeonToMIR
import Neon.MIRToMC as LIRToMC

import Neon.Config

import Text.Parsec (parse, ParseError)

data CompilerError = LexicalError LexicalError
                   | ParseError ParseError
                   | RenameError RenameError
                   | TypeError TypeError
                   deriving Show    

compileToMC :: Members '[Error CompilerError, Embed IO, Output LowerWarning] r => Text -> Text -> Sem r [(FilePath, Text)]
compileToMC name code = do
    let Config { printLir } = getConfig ()

    tokens <- mapError LexicalError $ lex code
    syntax <- mapError ParseError $ fromEither $ parse module_ "" tokens
    renamed <- mapError RenameError $ rename (emptyModuleEnv name) syntax
    typed <- mapError TypeError $ evalState (TCState mempty) $ typecheck renamed

    lir <- NeonToMIR.compile typed
    when printLir $ putTextLn $ pretty lir

    LIRToMC.compile lir 

        