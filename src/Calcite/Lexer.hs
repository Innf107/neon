module Calcite.Lexer (
    Token (..)
,   LexicalError (..)
,   lex
) where

import Calcite.Prelude

import Data.Text qualified as T
import Text.Builder qualified as B

import Data.Char

data LexicalError = UnexpectedChar Char deriving (Show, Eq)

reserved :: Set Text
reserved = fromList ["score", "int", "let", ";", ":", "=", "=>"]

data Token = TReserved Text
           | TReservedOp Text
           | TIdent Text
           | TOp Text
           | TParen Text
           | TIntLit Int
           deriving (Show, Eq)

pattern (:>) :: Char -> Text -> Text
pattern a :> as <- (T.uncons -> Just (a, as))
    where
        a :> as = T.cons a as

pattern EOF :: Text
pattern EOF <- (T.uncons -> Nothing)
    where
        EOF = T.empty
{-# COMPLETE (:>), EOF #-}

(+>) :: Applicative f => a -> f [a] -> f [a]
x +> xs = (x :) <$> xs
infixr 5 +>

lex :: Members '[Error LexicalError] r => Text -> Sem r [Token]
lex = lexDefault

lexDefault :: Members '[Error LexicalError] r => Text -> Sem r [Token]
lexDefault EOF = pure []
lexDefault (c :> cs)
    | isIdentStart c = lexIdent (B.char c) cs
    | isOpStart c = lexOp (B.char c) cs
    | isParen c = TParen (one c) +> lexDefault cs
    | isDigit c = lexIntLit (B.char c) cs
    | isSpace c = lexDefault cs
    | otherwise = throw (UnexpectedChar c)

lexIdent :: Members '[Error LexicalError] r => B.Builder -> Text -> Sem r [Token]
lexIdent b EOF = pure [buildIdent b]
lexIdent b (c :> cs)
    | isIdent c = lexIdent (b <> B.char c) cs
    | isOpStart c = buildIdent b +> lexOp (B.char c) cs
    | isParen c = buildIdent b +> TParen (one c) +> lexDefault cs
    | isSpace c = buildIdent b +> lexDefault cs
    | otherwise = throw (UnexpectedChar c)

buildIdent :: B.Builder -> Token
buildIdent b
    | built `member` reserved = TReserved built
    | otherwise               = TIdent built
    where
        built = B.run b

lexOp :: Members '[Error LexicalError] r => B.Builder -> Text -> Sem r [Token]
lexOp b EOF = pure [buildOp b]
lexOp b (c :> cs)
    | isOp c = lexOp (b <> B.char c) cs
    | isIdentStart c = buildOp b +> lexIdent (B.char c) cs
    | isParen c = buildOp b +> TParen (one c) +> lexDefault cs
    | isDigit c = buildOp b +> lexIntLit (B.char c) cs
    | isSpace c = buildOp b +> lexDefault cs
    | otherwise = throw (UnexpectedChar c)

buildOp :: B.Builder -> Token
buildOp b
    | built `member` reserved = TReservedOp built
    | otherwise               = TIdent built 
    where
        built = B.run b

lexIntLit :: Members '[Error LexicalError] r => B.Builder -> Text -> Sem r [Token]
lexIntLit b EOF = pure [buildIntLit b]
lexIntLit b (c :> cs)
    | isDigit c = lexIntLit (b <> B.char c) cs
    | isParen c = buildIntLit b +> TParen (one c) +> lexDefault cs
    | isIdent c = buildIntLit b +> lexIntLit (B.char c) cs
    | isOpStart c = buildIntLit b +> lexOp (B.char c) cs
    | isSpace c = buildIntLit b +> lexDefault cs
    | otherwise = throw (UnexpectedChar c)

buildIntLit :: B.Builder -> Token
buildIntLit b = case readMaybe (toString $ B.run b) of
   Nothing -> error $ "buildIntLit: expected an integer string, but received: " <> B.run b
   Just n  -> TIntLit n

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || (c `member` (fromList "_" :: Set Char))

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || (c `member` (fromList "_'" :: Set Char))

isOpStart :: Char -> Bool
isOpStart = isOp

isOp :: Char -> Bool
isOp = (`member` (fromList "+-=*/:;#?!$%&^.,<>|\\~" :: Set Char))

isParen :: Char -> Bool
isParen = (`member` (fromList "()[]{}" :: Set Char))

