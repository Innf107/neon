module Calcite.Types.AST where

import Calcite.Prelude
import Text.Show qualified as S

data Pass = Parsed | Renamed | Typed

type family XName (p :: Pass)

data Decl (p :: Pass) = DefFunction (XDefFunction p) (XName p) [(XName p, Type p)] (Type p) [Statement p] (Expr p)
                      | DefProc     (XDefProc p)     (XName p) [(XName p, Type p)]          [Statement p]
type family XDefFunction (p :: Pass)
type family XDefProc     (p :: Pass)

data Statement (p :: Pass) = DefVar (XDecl p) (XName p) (Expr p)
type family XDecl (p :: Pass)

data Expr (p :: Pass) = IntLit (XIntLit p) Int
                      | Var (XVar p) (XName p)
                      | FCall (XFCall p) (XName p) [Expr p]
                    -- XName p is fine here, since first class functions are not supported anyway.
type family XIntLit (p :: Pass)
type family XVar (p :: Pass)
type family XFCall (p :: Pass)

type role Type phantom
data Type (p :: Pass) = IntT 
                      | FunT [Type p] (Type p)
                      | ProcT [Type p]

data Name = Name {
        originalName :: Text
    ,   nameIndex :: Int
    } deriving (Eq, Ord)

renderName :: Name -> Text
renderName (Name originalName 0) = originalName
renderName (Name originalName nameIndex) = originalName <> "_" <> show nameIndex

instance Show Name where
    show = toString . renderName

data NoExt (p :: Pass) = NoExt deriving (Show, Eq)

type instance XName Parsed  = Text
type instance XName Renamed = Name
type instance XName Typed   = Name


type instance XDefFunction Parsed   = NoExt Parsed
type instance XDefFunction Renamed  = NoExt Renamed
type instance XDefFunction Typed    = NoExt Typed

type instance XDefProc Parsed       = NoExt Parsed
type instance XDefProc Renamed      = NoExt Renamed
type instance XDefProc Typed        = NoExt Typed

type instance XDecl         Parsed  = NoExt Parsed
type instance XDecl         Renamed = NoExt Renamed
type instance XDecl         Typed   = NoExt Typed

type instance XIntLit       Parsed  = NoExt Parsed
type instance XIntLit       Renamed = NoExt Renamed
type instance XIntLit       Typed   = NoExt Typed

type instance XVar          Parsed  = NoExt Parsed
type instance XVar          Renamed = NoExt Renamed
type instance XVar          Typed   = Type Typed

type instance XFCall        Parsed  = NoExt Parsed
type instance XFCall        Renamed = NoExt Renamed
type instance XFCall        Typed   = Type Typed


class HasType t p | t -> p where
    getType :: t -> Type p

instance HasType (Expr Typed) Typed where
    getType IntLit{}  = IntT
    getType (Var t _) = t
    getType (FCall t _ _) = t

class Cast a b where
    cast :: a -> b

instance Cast a b => Cast [a] [b] where
    cast = map cast

instance Cast (Type p1) (Type p2) where
    cast IntT        = IntT
    cast (FunT xs r) = FunT (cast xs) (cast r)
    cast (ProcT xs)  = ProcT (cast xs)



instance Show (Type p) where
    show IntT        = "int"
    show (FunT as r) = toString $ "(" <> intercalate ", " (map show as) <> ") -> " <> show r
    show (ProcT xs)  = toString $ "(" <> intercalate ", " (map show xs) <> ") -> void"

deriving instance Eq (Type p)
