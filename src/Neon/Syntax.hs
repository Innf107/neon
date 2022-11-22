module Neon.Syntax where

import Neon.Prelude
import Text.Show qualified as S
import Neon.Pretty

data Pass = Parsed | Renamed | Typed

type family XName (p :: Pass)

data Decl (p :: Pass) = DefFunction (XDefFunction p) (XName p) [(XName p, Type)] Type [Statement p] (Expr p)
type family XDefFunction (p :: Pass)

data Statement (p :: Pass) 
    = DefVar (XDefVar p) (XName p) (Expr p)
    -- | Perform an expression for its side effect while ignoring the result
    | Perform (XPerform p) (Expr p)
    | InlineAsm (XInlineAsm p) (Seq (InlineAsmComponent p))

type family XDefVar (p :: Pass)
type family XPerform (p :: Pass)
type family XInlineAsm (p :: Pass)

data InlineAsmComponent (p :: Pass)
    = AsmText (XAsmText p) Text

type family XAsmText (p :: Pass)

data Expr (p :: Pass) = IntLit (XIntLit p) Int
                      | UnitLit (XUnitLit p)
                      | Var (XVar p) (XName p)
                      | FCall (XFCall p) (XName p) [Expr p]
                      | BinOp (XBinOp p) (Expr p) BinOp (Expr p)
                      | Return (XReturn p) (Expr p)
                      | ExprBlock (XExprBlock p) (Seq (Statement p)) (Expr p)
                      | If (XIf p) (Expr p) (Expr p) (Expr p)

type family XIntLit (p :: Pass)
type family XUnitLit (p :: Pass)
type family XVar (p :: Pass)
type family XFCall (p :: Pass)
type family XBinOp (p :: Pass)
type family XReturn (p :: Pass)
type family XExprBlock (p :: Pass)
type family XIf (p :: Pass)

data BinOp = Add 
           | LE
           deriving (Show)

data Type = IntT
          | BoolT
          | UnitT
          -- | A type representing diverging computations (such as 'return').
          -- never is a mutual subtype of every type.
          -- If we get unification later, never will trivially *unify* with every type
          | NeverT

data Name = Name {
        originalName :: Text
    ,   nameSource :: Text
    ,   nameIndex :: Int
    } deriving (Eq, Ord)
instance Pretty Name where pretty = show

renderName :: Name -> Text
renderName name@(Name _ nameSource _) = nameSource <> ":" <> renderNameNoPrefix name

renderNameNoPrefix :: Name -> Text
renderNameNoPrefix (Name originalName _nameSource 0) = originalName
renderNameNoPrefix (Name originalName _nameSource nameIndex) = originalName <> "_" <> show nameIndex

instance Show Name where
    show = toString . renderName

type instance XName Parsed  = Text
type instance XName Renamed = Name
type instance XName Typed   = Name


type instance XDefFunction Parsed   = ()
type instance XDefFunction Renamed  = ()
type instance XDefFunction Typed    = ()

type instance XDefVar       Parsed  = ()
type instance XDefVar       Renamed = ()
type instance XDefVar       Typed   = ()

type instance XPerform      Parsed  = ()
type instance XPerform      Renamed = ()
type instance XPerform      Typed   = ()

type instance XInlineAsm    Parsed  = ()
type instance XInlineAsm    Renamed = ()
type instance XInlineAsm    Typed   = ()

type instance XAsmText      Parsed  = ()
type instance XAsmText      Renamed = ()
type instance XAsmText      Typed   = ()


type instance XUnitLit      Parsed  = ()
type instance XUnitLit      Renamed = ()
type instance XUnitLit      Typed   = ()

type instance XIntLit       Parsed  = ()
type instance XIntLit       Renamed = ()
type instance XIntLit       Typed   = ()

type instance XVar          Parsed  = ()
type instance XVar          Renamed = ()
type instance XVar          Typed   = Type

type instance XFCall        Parsed  = ()
type instance XFCall        Renamed = ()
type instance XFCall        Typed   = Type

type instance XBinOp        Parsed  = ()
type instance XBinOp        Renamed = ()
type instance XBinOp        Typed   = Type

type instance XReturn       Parsed  = ()
type instance XReturn       Renamed = ()
type instance XReturn       Typed   = ()

type instance XExprBlock    Parsed  = ()
type instance XExprBlock    Renamed = ()
type instance XExprBlock    Typed   = ()

type instance XIf           Parsed  = ()
type instance XIf           Renamed = ()
type instance XIf           Typed   = Type

class HasType t where
    getType :: t -> Type

instance HasType (Expr Typed) where
    getType IntLit{}  = IntT
    getType UnitLit{} = UnitT
    getType (Var t _) = t
    getType (FCall t _ _) = t
    getType (BinOp t _ _ _) = t 
    getType (Return _ _) = NeverT
    getType (ExprBlock _ _ retExpr) = getType retExpr
    getType (If t _ _ _) = t

class Cast a b where
    cast :: a -> b

instance Cast a b => Cast [a] [b] where
    cast = map cast


instance Show Type where
    show IntT        = "int"
    show BoolT       = "bool"
    show UnitT       = "unit"
    show NeverT      = "!"

deriving instance Eq Type
