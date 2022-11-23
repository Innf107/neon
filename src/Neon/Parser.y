{
{-# LANGUAGE NoStrictData #-}
module Neon.Parser (parse) where

import Prelude

import Neon.Prelude (fromMaybe, Seq, (<|))

import Data.Text (Text)

import Neon.Syntax
import Neon.Lexer (Token(..), TokenClass(..))

import Neon.Parser.Util

import GHC.Exts (fromList)
}

%name parse Decls
%tokentype { Token }
%error { parseError }

%token identS           { (IdentToken $$) }
%token intLitS          { (IntLitToken $$) }
%token let              { (Token LET _) }
%token '='              { (Token EQUALS _) }
%token if               { (Token IF _) }
%token else             { (Token ELSE _) }
%token '('              { (Token LPAREN _) }
%token ')'              { (Token RPAREN _) }
%token '{'              { (Token LBRACE _) }
%token '}'              { (Token RBRACE _) }
%token ':'              { (Token COLON _) }
%token ','              { (Token COMMA _) }
%token int              { (Token INT _) }
%token bool             { (Token BOOL _) }
%token return           { (Token RETURN _) }
%token ';'              { (Token SEMI _) }
%token '+'              { (Token PLUS _) }
%token '<='             { (Token LEOP _) }
%token '[asm|'          { (Token OPENINLINEASM _) }
%token '|]'             { (Token CLOSEINLINEASM _) }
%token asmTextS         { (InlineAsmTextToken $$) }
%token '$<'             { (Token OPENASMINTERP _) }
%token '>$'             { (Token CLOSEASMINTERP _) }

%left '<='
%left '+'

%%

-- Defined for convenience to allow omitting unnecessary spans from indentifiers  
ident :: { Text }
ident : identS { fst $1 }
intLit :: { Int }
intLit : intLitS { fst $1 }
asmText :: { Text }
asmText : asmTextS { fst $1 }

-- Left associative grammars are much more efficient for a shift/reduce parser,
-- so we define some and many this way and reverse the list afterwards
rev_some(p) : p                 { [$1] }
            | rev_some(p) p     { $2 : $1 }

some(p) : rev_some(p)   { reverse $1 }

many(p) : some(p)   { $1 }
        |           { [] }

maybe(p) : p        { Just $1 }
         |          { Nothing }

Decls :: { [Decl Parsed] }
Decls : many(Decl) { $1 }

Decl :: { Decl Parsed }
Decl : ident '(' ParameterList ')' maybe(TypeSig) '{' Body '}' 
        { DefFunction () $1 $3 (fromMaybe UnitT $5) (fst $7) (fromMaybe (UnitLit ()) (snd $7)) }

TypeSig : ':' Type { $2 }

ParameterList :: { [(Text, Type)] }
ParameterList : ident ':' Type ',' ParameterList { ($1, $3) : $5 }
              | ident ':' Type                   { [($1, $3)] }
              |                                  { [] }

Body :: { ([Statement Parsed], Maybe (Expr Parsed)) }
Body : Statement ';' Body   { let (stmnts, ret) = $3 in ($1 : stmnts, ret) }
     | Expr                 { ([], Just $1) }
     |                      { ([], Nothing) }

Statement :: { Statement Parsed }
Statement : let ident '=' Expr          { DefVar () $2 $4 }
          | Expr                        { Perform () $1 }
          | '[asm|' InlineAsmBody '|]'  { InlineAsm () $2 }

InlineAsmBody :: { Seq (InlineAsmComponent Parsed) }
InlineAsmBody : asmText InlineAsmBody           { AsmText () $1 <| $2 }
              | '$<' Expr '>$' InlineAsmBody    { AsmInterpolation () $2 <| $4 }
              |                                 { [] }

Expr :: { Expr Parsed }
Expr : '(' Expr ')'                 { $2 }
     | intLit                       { IntLit () $1 }
     | ident                        { Var () $1 }
     | ident '(' ArgumentList ')'   { FCall () $1 $3 }
     | Expr '+' Expr                { BinOp () $1 Add $3 }
     | Expr '<=' Expr               { BinOp () $1 LE $3 }
     | return Expr                  { Return () $2 }
     | '{' ExprBlockBody '}'        { let (stmnts, ret) = $2 in ExprBlock () (fromList stmnts) ret }
     | if Expr '{' ExprBlockBody '}' else '{' ExprBlockBody '}' 
        { If () $2 (collapseExprBlock $4) (collapseExprBlock $8) }

ExprBlockBody :: { ([Statement Parsed], Expr Parsed) }
ExprBlockBody : Statement ';' ExprBlockBody     { let (stmnts, ret) = $3 in ($1 : stmnts, ret) }
              | Expr                            { ([], $1) }

ArgumentList : Expr ',' ArgumentList    { $1 : $3 }
             | Expr                     { [$1] }
             |                          { [] }

Type :: { Type }
Type : int     { IntT }
     | bool    { BoolT }
     | '(' ')' { UnitT }



{
parseError :: [Token] -> a
parseError [] = error "Parse Error: Unexpected EOF" 
parseError (tok : tokens) = error $ show (tokenSpan tok) <> ": Parse Error: Unexpected tokens '" 
      <> show (map tokenClass (tok : tokens)) <> "'"
}