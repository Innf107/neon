module Neon.Parser where

import Neon.Lexer
import Neon.Prelude hiding (many, (<|>))
import Neon.Syntax

import Text.Parsec as P
import Text.Parsec.Pos as P

type Parser = Parsec [Token] ()

(<??>) :: String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)

infixr 0 <??>

module_ :: Parser [Decl Parsed]
module_ = many (decl <* reservedOp ";") <* eof

decl :: Parser (Decl Parsed)
decl = "declaration" <??> defFunction

defFunction :: Parser (Decl Parsed)
defFunction = "function definition" <??> join $
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
statement = "statement" <??> 
          (  defVar <* reservedOp ";"
         <|> Perform () <$> P.try (expr <* reservedOp ";"))
                
defVar :: Parser (Statement Parsed)
defVar = "variable definition" <??>
    DefVar ()
        <$ reserved "let"
        <*> ident
        <* reservedOp "="
        <*> expr

expr :: Parser (Expr Parsed)
expr = "expression" <??> (intLit <|> returnExp <|> exprBlock <|> binOp <|> fcallOrVar)

binOp :: Parser (Expr Parsed)
binOp = do
    -- TODO: binary operators are currently parsed right associatively... ugh
    (left, op) <- P.try $ (,) <$> fcallOrVar <*> op
    right <- expr
    pure (BinOp () left op right)
        where
            op = Add <$ reservedOp "+"

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

returnExp :: Parser (Expr Parsed)
returnExp = (\_ e -> Return () e) <$> reserved "return" <*> expr

exprBlock :: Parser (Expr Parsed)
exprBlock = "expression block" <??> (\_ statements -> \case
                Just retExpr -> ExprBlock () (fromList statements) retExpr
                Nothing -> undefined)
        <$> paren "{"
        <*> many statement
        <*> ( Just <$> expr <* paren "}"
                <|> pure Nothing <* paren "}"
            )


intLit :: Parser (Expr Parsed)
intLit =
    IntLit ()
        <$> intLitT

type_ :: Parser Type
type_ = reserved "int" $> IntT

token' :: (Token -> Maybe a) -> Parser a
token' = token show (\_ -> newPos "" 0 0)

reserved :: Text -> Parser Text
reserved r = toString r <??> token' \case
    TReserved x | x == r -> Just x
    _ -> Nothing

reservedOp :: Text -> Parser Text
reservedOp r = toString r <??> token' \case
    TReservedOp x | x == r -> Just x
    _ -> Nothing

ident :: Parser Text
ident = "identifier" <??> token' \case
    TIdent x -> Just x
    _ -> Nothing

op :: Parser Text
op = "operator" <??> token' \case
    TOp x -> Just x
    _ -> Nothing

paren :: Text -> Parser Text
paren r = toString r <??> token' \case
    TParen x | x == r -> Just x
    _ -> Nothing

intLitT :: Parser Int
intLitT = "integer" <??> token' \case
    TIntLit x -> Just x
    _ -> Nothing