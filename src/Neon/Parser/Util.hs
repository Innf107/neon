module Neon.Parser.Util (
    withSpan
,   pattern IdentToken
,   pattern IntLitToken
,   makeRetExpr
,   collapseExprBlock
) where

import Neon.Lexer
import Neon.Prelude

import Neon.Syntax (Statement, Expr (ExprBlock), Type, Pass(Parsed))
import GHC.Base (errorWithoutStackTrace)

-- This is technically a bit unsafe, since it only works correctly if the first span ends before the second one,
-- so we really should not put it in @Neon.Span@.
withSpan :: (Spanned a, Spanned b) => a -> b -> Span
withSpan a b =
    let s1 = spanOf a
     in let s2 = spanOf b
         in UnsafeMkSpan
                { sourceFile = sourceFile s1
                , startLine = startLine s1
                , startCol = startCol s1
                , endLine = endLine s2
                , endCol = endCol s2
                }

-- Workaround to return multiple values in a Happy token pattern
asIdentToken :: Token -> Maybe (Text, Span)
asIdentToken (Token (IDENT ident) tokenSpan) = Just (ident, tokenSpan)
asIdentToken _ = Nothing

pattern IdentToken :: (Text, Span) -> Token
pattern IdentToken t <- (asIdentToken -> Just t)

asIntLitToken :: Token -> Maybe (Int, Span)
asIntLitToken (Token (INTLIT lit) tokenSpan) = Just (lit, tokenSpan)
asIntLitToken _ = Nothing

pattern IntLitToken :: (Int, Span) -> Token
pattern IntLitToken t <- (asIntLitToken -> Just t)


makeRetExpr :: Maybe (Expr Parsed) -> Maybe Type -> Maybe (Expr Parsed, Type)
makeRetExpr Nothing Nothing = Nothing
makeRetExpr (Just expr) (Just ty) = Just (expr, ty)
makeRetExpr (Just _expr) Nothing = errorWithoutStackTrace "A function without a return type cannot contain a return expression (This restriction might be lifted in the future)"
makeRetExpr Nothing (Just _ty) = errorWithoutStackTrace "A function with a specified return type needs to return a value"

collapseExprBlock :: ([Statement Parsed], Expr Parsed) -> Expr Parsed
collapseExprBlock ([], expr) = expr
collapseExprBlock (stmnts, expr) = ExprBlock () (fromList stmnts) expr

