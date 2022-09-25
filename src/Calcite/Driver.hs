module Calcite.Driver where

import Calcite.Prelude
import Calcite.Types.AST

import Calcite.Lexer
import Calcite.Parser
import Calcite.Rename
import Calcite.Typecheck
import Calcite.Codegen
import Calcite.Packager

import Text.Parsec (parse, ParseError)

import Language.McFunction.Types

data CompilerError = LexicalError LexicalError
                   | ParseError ParseError
                   | RenameError RenameError
                   | TypeError TypeError
                   deriving (Show,Eq)    

lexAndParse :: Members '[Error CompilerError] r => Text -> Sem r [Decl 'Parsed]
lexAndParse code = do
    tokens <- mapError LexicalError $ lex code
    mapError ParseError $ fromEither $ parse module_ "" tokens

compileToRename :: Members '[Error CompilerError] r => Text -> Text -> Sem r [Decl 'Renamed]
compileToRename name code = do
    ast <- lexAndParse code
    mapError RenameError $ rename (emptyModuleEnv name) ast
    
compileToTypecheck :: Members '[Error CompilerError] r => Text -> Text -> Sem r [Decl 'Typed]
compileToTypecheck name code = do
    ast <- compileToRename name code
    mapError TypeError $ evalState (TCState mempty) $ typecheck ast
    
compileToCodegen :: Members '[Error CompilerError] r => Text -> Text -> Sem r [CompiledModule]
compileToCodegen name code = do
    ast <- compileToTypecheck name code
    compile ast

compileToDatapack :: Members '[Error CompilerError] r => Text -> Text -> Sem r Datapack
compileToDatapack name code = package name <$> compileToCodegen name code

compileToZip :: Members '[Error CompilerError] r => Text -> Text -> Sem r Archive
compileToZip name code = datapackToZip <$> compileToDatapack name code

compileToZipLBytestring :: Members '[Error CompilerError] r => Text -> Text -> Sem r LByteString
compileToZipLBytestring name code = fromArchive <$> compileToZip name code
