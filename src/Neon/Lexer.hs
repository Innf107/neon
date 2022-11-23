module Neon.Lexer 
    (   Token (..)
    ,   TokenClass (..)
    ,   lex
    ,   LexError (..)
    ,   InterpolationLevel(..)
    ) where

import Neon.Prelude
import Prelude qualified

import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.List qualified as List



data LexError = InvalidChar Char 
              | UnexpectedEOF
              | UnclosedInterpolationLevels [InterpolationLevel]
              deriving (Show)

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
    | OPENASMINTERP
    | CLOSEASMINTERP
    deriving (Show, Eq)

data Token = Token {tokenClass :: TokenClass, tokenSpan :: Span}


instance Spanned Token where
    spanOf Token{tokenSpan} = tokenSpan

data LexState = Default 
              | InIdent [Char] 
              | InIntLit [Char]
              | InLineComment
              | InlineAsmDefault [Char]

data InterpolationLevel
    = InlineAsmLevel
    -- This is kept fairly generic to possibly allow string interpolation operators in the future.
    -- String interpolation specifically might be a bit complicated to implement, but
    -- the possibility of interpolation might still be useful in the future.
    deriving (Show)

tryFinishInterp :: Char -> Span -> [InterpolationLevel] -> Maybe ([Token], LexState, [InterpolationLevel])
tryFinishInterp _ _ [] = Nothing
tryFinishInterp '$' span (InlineAsmLevel : levels) = 
    Just ([Token CLOSEASMINTERP (inc span)], InlineAsmDefault [], levels)
tryFinishInterp _ _ (InlineAsmLevel : _) = Nothing


reserved :: Map Text TokenClass
reserved = [("let", LET), ("if", IF), ("else", ELSE), ("int", INT), ("bool", BOOL), ("return", RETURN)]

inc :: Span -> Span
inc span@UnsafeMkSpan{endCol} = span{endCol = endCol + 1}

incN :: Int -> Span -> Span
incN 0 span = span
incN n span = inc (incN (n - 1) span)

incLine :: Span -> Span
incLine span@UnsafeMkSpan{endLine} = span{endLine = endLine + 1, endCol = 1}

resetStart :: Span -> Span
resetStart UnsafeMkSpan{sourceFile, endLine, endCol} = UnsafeMkSpan{sourceFile, startLine = endLine, startCol = endCol, endLine, endCol}

lex :: forall r. Members '[Error LexError] r => FilePath -> Text -> Sem r [Token]
lex filePath = go (UnsafeMkSpan filePath 1 1 1 1) Default ([] :: [InterpolationLevel])
  where
    go oldSpan Default interpLevels input =
        let span = resetStart oldSpan
        in case Text.uncons input of
                Nothing -> case interpLevels of
                    [] -> pure []
                    lvls -> throw (UnclosedInterpolationLevels lvls)
                Just (c, rest) -> case c of
                    _ | Just (tokens, newState, newLevels) <- tryFinishInterp c span interpLevels
                        -> (tokens <>) <$> go (inc span) newState newLevels rest
                    '=' -> (Token EQUALS (inc span) :) <$> go (inc span) Default interpLevels rest
                    '(' -> (Token LPAREN (inc span) :) <$> go (inc span) Default interpLevels rest
                    ')' -> (Token RPAREN (inc span) :) <$> go (inc span) Default interpLevels rest
                    '{' -> (Token LBRACE (inc span) :) <$> go (inc span) Default interpLevels rest
                    '}' -> (Token RBRACE (inc span) :) <$> go (inc span) Default interpLevels rest
                    '[' | Just newRest <- Text.stripPrefix "asm|" rest ->
                        (Token OPENINLINEASM (incN 4 span) :) <$> go (inc (inc span)) (InlineAsmDefault []) interpLevels newRest
                    ':' -> (Token COLON  (inc span) :) <$> go (inc span) Default interpLevels rest
                    ',' -> (Token COMMA  (inc span) :) <$> go (inc span) Default interpLevels rest
                    ';' -> (Token SEMI   (inc span) :) <$> go (inc span) Default interpLevels rest
                    '+' -> (Token PLUS   (inc span) :) <$> go (inc span) Default interpLevels rest
                    '<' | Just newRest <- Text.stripPrefix "=" rest -> 
                        (Token LEOP (inc (inc span)) :) <$> go (inc (inc span)) Default interpLevels newRest
                    '-' | Just newRest <- Text.stripPrefix "-" rest -> go (inc (inc span)) InLineComment interpLevels newRest
                    '\n' -> go (incLine span) Default interpLevels rest
                    _ | Char.isSpace c -> go (inc span) Default interpLevels rest
                    _ | Char.isDigit c -> go (inc span) (InIntLit (one c)) interpLevels rest
                    _ | Char.isAlpha c -> go (inc span) (InIdent (one c)) interpLevels rest
                    _ -> throw @_ @r (InvalidChar c)
    go span (InIdent ident) interpLevels input = case Text.uncons input of
        Nothing -> 
            case interpLevels of
                [] -> pure [buildIdent span ident]
                lvls -> throw (UnclosedInterpolationLevels lvls)
        Just (c, rest) -> case c of
            _ | Char.isAlphaNum c -> go (inc span) (InIdent (c : ident)) interpLevels rest
            _ -> (buildIdent (inc span) ident :) <$> go (inc span) Default interpLevels input
      where
        -- buildIdent :: Span -> [Char] -> Token
        buildIdent span str =
            let ident = toText (List.reverse str)
             in case lookup ident reserved of
                    Nothing -> Token (IDENT ident) span
                    Just x -> Token x span
    go span (InIntLit lit) interpLevels input = case Text.uncons input of
        Nothing ->
            case interpLevels of
                [] -> pure [buildIntLit span lit]
                lvls -> throw (UnclosedInterpolationLevels lvls) 
        Just (c, rest) -> case c of
            _ | Char.isDigit c -> go (inc span) (InIntLit (c : lit)) interpLevels rest
            _ -> (buildIntLit (inc span) lit :) <$> go (inc span) Default interpLevels input
        where
            buildIntLit span str = Token (INTLIT (Prelude.read (List.reverse str))) span
    go span InLineComment interpLevels input = case Text.uncons input of
        Nothing ->
            case interpLevels of
                [] -> pure []
                lvls -> throw (UnclosedInterpolationLevels lvls)
        Just (c, rest) -> case c of
            '\n' -> go (incLine span) Default interpLevels rest
            _ -> go (inc span) InLineComment interpLevels rest

    go span (InlineAsmDefault lit) interpLevels input = case Text.uncons input of
        Nothing -> throw UnexpectedEOF
        Just (c, rest) -> case c of
            '|' | Just newRest <- Text.stripPrefix "]" rest ->
                let addFinalText = case lit of
                        [] -> id
                        _ -> (Token (INLINEASMTEXT (fromString (List.reverse lit))) span :)
                in
                addFinalText . (Token CLOSEINLINEASM (inc (inc span)) :) <$> go (inc (inc span)) Default interpLevels newRest
            '$' -> (Token OPENASMINTERP (inc span) :) <$> go (inc span) Default (InlineAsmLevel : interpLevels) rest
            _ -> go (inc span) (InlineAsmDefault (c : lit)) interpLevels rest