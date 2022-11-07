module Neon.Syntax where

import Neon.Prelude
import Text.Show qualified as S
import Neon.Pretty

data Pass = Parsed | Renamed | Typed

type family XName (p :: Pass)

data Decl (p :: Pass) = DefFunction (XDefFunction p) (XName p) [(XName p, Type)] [Statement p] (Maybe (Expr p, Type))
type family XDefFunction (p :: Pass)

data Statement (p :: Pass) 
    = DefVar (XDefVar p) (XName p) (Expr p)
    -- | Perform an expression for its side effect while ignoring the result
    | Perform (XPerform p) (Expr p)

type family XDefVar (p :: Pass)
type family XPerform (p :: Pass)

data Expr (p :: Pass) = IntLit (XIntLit p) Int
                      | Var (XVar p) (XName p)
                      | FCall (XFCall p) (XName p) [Expr p]
                      | BinOp (XBinOp p) (Expr p) BinOp (Expr p)
                      | Return (XReturn p) (Expr p)
                      | ExprBlock (XExprBlock p) (Seq (Statement p)) (Expr p)

type family XIntLit (p :: Pass)
type family XVar (p :: Pass)
type family XFCall (p :: Pass)
type family XBinOp (p :: Pass)
type family XReturn (p :: Pass)
type family XExprBlock (p :: Pass)

data BinOp = Add deriving (Show)

data Type = IntT
          | FunT [Type] Type
          | ProcT [Type]
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

class HasType t where
    getType :: t -> Type

instance HasType (Expr Typed) where
    getType IntLit{}  = IntT
    getType (Var t _) = t
    getType (FCall t _ _) = t
    getType (BinOp t _ _ _) = t 
    getType (Return _ _) = NeverT
    getType (ExprBlock _ _ retExpr) = getType retExpr

class Cast a b where
    cast :: a -> b

instance Cast a b => Cast [a] [b] where
    cast = map cast


instance Show Type where
    show IntT        = "int"
    show (FunT as r) = toString $ "(" <> intercalate ", " (map show as) <> ") -> " <> show r
    show (ProcT xs)  = toString $ "(" <> intercalate ", " (map show xs) <> ") -> void"
    show NeverT      = "!"

deriving instance Eq Type
