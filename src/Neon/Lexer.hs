module Neon.Lexer (Token (..), TokenClass (..), lex, LexError (..)) where

import Neon.Prelude
import Prelude qualified

import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.List qualified as List



data LexError = InvalidChar Char 
              | UnexpectedEOF
              deriving (Show, Eq)

data TokenClass
    = IDENT Text
    | INTLIT Int
    | LET
    | EQUALS
    | IF
    | ELSE
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | COLON
    | COMMA
    | INT -- TODO: Maybe int shouldn't be a keyword?
    | BOOL
    | RETURN
    | SEMI
    | PLUS
    | LEOP
    | OPENINLINEASM
    | INLINEASMTEXT Text
    | CLOSEINLINEASM
    deriving (Show, Eq)

data Token = Token {tokenClass :: TokenClass, tokenSpan :: Span}


instance Spanned Token where
    spanOf Token{tokenSpan} = tokenSpan

data LexState = Default 
              | InIdent [Char] 
              | InIntLit [Char]
              | InLineComment
              | InlineAsmDefault [Char]

reserved :: Map Text TokenClass
reserved = [("let", LET), ("if", IF), ("else", ELSE), ("int", INT), ("bool", BOOL), ("return", RETURN)]

inc :: Span -> Span
inc span@UnsafeMkSpan{endCol} = span{endCol = endCol + 1}

incLine :: Span -> Span
incLine span@UnsafeMkSpan{endLine} = span{endLine = endLine + 1, endCol = 1}

resetStart :: Span -> Span
resetStart UnsafeMkSpan{sourceFile, endLine, endCol} = UnsafeMkSpan{sourceFile, startLine = endLine, startCol = endCol, endLine, endCol}

lex :: forall r. Members '[Error LexError] r => FilePath -> Text -> Sem r [Token]
lex filePath = go (UnsafeMkSpan filePath 1 1 1 1) Default
  where
    go oldSpan Default input =
        let span = resetStart oldSpan
         in case Text.uncons input of
                Nothing -> pure []
                Just (c, rest) -> case c of
                    '=' -> (Token EQUALS (inc span) :) <$> go (inc span) Default rest
                    '(' -> (Token LPAREN (inc span) :) <$> go (inc span) Default rest
                    ')' -> (Token RPAREN (inc span) :) <$> go (inc span) Default rest
                    '{' -> (Token LBRACE (inc span) :) <$> go (inc span) Default rest
                    '}' -> (Token RBRACE (inc span) :) <$> go (inc span) Default rest
                    '[' | Just newRest <- Text.stripPrefix "|" rest ->
                        (Token OPENINLINEASM (inc (inc span)) :) <$> go (inc (inc span)) (InlineAsmDefault []) newRest
                    ':' -> (Token COLON  (inc span) :) <$> go (inc span) Default rest
                    ',' -> (Token COMMA  (inc span) :) <$> go (inc span) Default rest
                    ';' -> (Token SEMI   (inc span) :) <$> go (inc span) Default rest
                    '+' -> (Token PLUS   (inc span) :) <$> go (inc span) Default rest
                    '<' | Just newRest <- Text.stripPrefix "=" rest -> 
                        (Token LEOP (inc (inc span)) :) <$> go (inc (inc span)) Default newRest
                    '-' | Just newRest <- Text.stripPrefix "-" rest -> go (inc (inc span)) InLineComment newRest
                    '\n' -> go (incLine span) Default rest
                    _ | Char.isSpace c -> go (inc span) Default rest
                    _ | Char.isDigit c -> go (inc span) (InIntLit (one c)) rest
                    _ | Char.isAlpha c -> go (inc span) (InIdent (one c)) rest
                    _ -> throw @_ @r (InvalidChar c)
    go span (InIdent ident) input = case Text.uncons input of
        Nothing -> pure [buildIdent span ident]
        Just (c, rest) -> case c of
            _ | Char.isAlphaNum c -> go (inc span) (InIdent (c : ident)) rest
            _ -> (buildIdent (inc span) ident :) <$> go (inc span) Default input
      where
        -- buildIdent :: Span -> [Char] -> Token
        buildIdent span str =
            let ident = toText (List.reverse str)
             in case lookup ident reserved of
                    Nothing -> Token (IDENT ident) span
                    Just x -> Token x span
    go span (InIntLit lit) input = case Text.uncons input of
        Nothing -> pure [buildIntLit span lit]
        Just (c, rest) -> case c of
            _ | Char.isDigit c -> go (inc span) (InIntLit (c : lit)) rest
            _ -> (buildIntLit (inc span) lit :) <$> go (inc span) Default input
        where
            buildIntLit span str = Token (INTLIT (Prelude.read (List.reverse str))) span
    go span InLineComment input = case Text.uncons input of
        Nothing -> pure []
        Just (c, rest) -> case c of
            '\n' -> go (incLine span) Default rest
            _ -> go (inc span) InLineComment rest

    go span (InlineAsmDefault lit) input = case Text.uncons input of
        Nothing -> throw UnexpectedEOF
        Just (c, rest) -> case c of
            '|' | Just newRest <- Text.stripPrefix "]" rest ->
                let addFinalText = case lit of
                        [] -> id
                        _ -> (Token (INLINEASMTEXT (fromString (List.reverse lit))) span :)
                in
                addFinalText . (Token CLOSEINLINEASM (inc (inc span)) :) <$> go (inc (inc span)) Default newRest
            -- TODO: Not quite sure how to do this yet
            '{' -> error "Inline asm interpolation is NYI"
            _ -> go (inc span) (InlineAsmDefault (c : lit)) rest