module Calcite.Driver ( 
      CompilerError(..)
    , LowerWarning (..)
    , compileToMC
    ) where

import Calcite.Prelude
import Calcite.Types.AST
import Calcite.Pretty

import Calcite.Lexer
import Calcite.Parser
import Calcite.Rename
import Calcite.Typecheck
import Calcite.Packager

import Calcite.MIR
import Calcite.CalciteToMIR as CalciteToMIR
import Calcite.MIRToMC as LIRToMC

import Calcite.Config

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

    lir <- CalciteToMIR.compile typed
    when printLir $ putTextLn $ pretty lir

    LIRToMC.compile lir 

        