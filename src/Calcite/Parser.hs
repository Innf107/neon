module Calcite.Parser where

import Calcite.Lexer
import Calcite.Prelude hiding (many, (<|>))
import Calcite.Types.AST

import Text.Parsec as P
import Text.Parsec.Pos as P

type Parser = Parsec [Token] ()

module_ :: Parser [Decl Parsed]
module_ = many (decl <* reservedOp ";") <* eof

decl :: Parser (Decl Parsed)
decl = defFunction

defFunction :: Parser (Decl Parsed)
defFunction = join $
    (\x args mty sts retExpr -> 
        case (mty, retExpr) of
            (Nothing, Nothing) -> pure $ DefFunction () x args sts Nothing
            (Just ty, Just expr) -> pure $ DefFunction () x args sts (Just (expr, ty))
            (Just _, Nothing) -> fail "Missing return expression in non-void function"
            (Nothing, Just _) -> fail "Invalid return expression in void function")
        <$> ident
        <* paren "("
        <*> typedIdent `sepBy` reservedOp ","
        <* paren ")"
        <*> P.optionMaybe (
            reservedOp ":"
             *> type_
        )
        <* paren "{"
        <*> many statement
        <*> ( Just <$> expr <* paren "}"
                <|> pure Nothing <* paren "}"
            )

typedIdent :: Parser (Text, Type)
typedIdent =
    (,)
        <$> ident
        <* reservedOp ":"
        <*> type_

statement :: Parser (Statement Parsed)
statement = defVar <* reservedOp ";"

defVar :: Parser (Statement Parsed)
defVar =
    DefVar ()
        <$ reserved "let"
        <*> ident
        <* reservedOp "="
        <*> expr

expr :: Parser (Expr Parsed)
expr = intLit <|> fcallOrVar

fcallOrVar :: Parser (Expr Parsed)
fcallOrVar =
    (&)
        <$> ident
        <*> (fcall <|> pure (Var ()))
  where
    fcall =
        (\args f -> FCall () f args)
            <$ paren "("
            <*> expr `sepBy` reservedOp ","
            <* paren ")"

intLit :: Parser (Expr Parsed)
intLit =
    IntLit ()
        <$> intLitT

type_ :: Parser Type
type_ = reserved "int" $> IntT

token' :: (Token -> Maybe a) -> Parser a
token' = token show (\_ -> newPos "" 0 0)

reserved :: Text -> Parser Text
reserved r = token' \case
    TReserved x | x == r -> Just x
    _ -> Nothing

reservedOp :: Text -> Parser Text
reservedOp r = token' \case
    TReservedOp x | x == r -> Just x
    _ -> Nothing

ident :: Parser Text
ident = token' \case
    TIdent x -> Just x
    _ -> Nothing

op :: Parser Text
op = token' \case
    TOp x -> Just x
    _ -> Nothing

paren :: Text -> Parser Text
paren r = token' \case
    TParen x | x == r -> Just x
    _ -> Nothing

intLitT :: Parser Int
intLitT = token' \case
    TIntLit x -> Just x
    _ -> Nothing
