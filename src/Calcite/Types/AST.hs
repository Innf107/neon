module Calcite.Types.AST where

import Calcite.Prelude
import Text.Show qualified as S

data Pass = Parsed | Renamed | Typed

type family XName (p :: Pass)

data Decl (p :: Pass) = DefFunction (XDefFunction p) (XName p) [(XName p, Type)] [Statement p] (Maybe (Expr p, Type))
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

data Type = IntT
          | FunT [Type] Type
          | ProcT [Type]

data Name = Name {
        originalName :: Text
    ,   nameSource :: Text
    ,   nameIndex :: Int
    } deriving (Eq, Ord)

renderName :: Name -> Text
renderName (Name originalName nameSource 0) = nameSource <> ":" <> originalName
renderName (Name originalName nameSource nameIndex) = nameSource <> ":" <> originalName <> "_" <> show nameIndex

instance Show Name where
    show = toString . renderName

type instance XName Parsed  = Text
type instance XName Renamed = Name
type instance XName Typed   = Name


type instance XDefFunction Parsed   = ()
type instance XDefFunction Renamed  = ()
type instance XDefFunction Typed    = ()

type instance XDefProc Parsed       = ()
type instance XDefProc Renamed      = ()
type instance XDefProc Typed        = ()

type instance XDecl         Parsed  = ()
type instance XDecl         Renamed = ()
type instance XDecl         Typed   = ()

type instance XIntLit       Parsed  = ()
type instance XIntLit       Renamed = ()
type instance XIntLit       Typed   = ()

type instance XVar          Parsed  = ()
type instance XVar          Renamed = ()
type instance XVar          Typed   = Type

type instance XFCall        Parsed  = ()
type instance XFCall        Renamed = ()
type instance XFCall        Typed   = Type


class HasType t where
    getType :: t -> Type

instance HasType (Expr Typed) where
    getType IntLit{}  = IntT
    getType (Var t _) = t
    getType (FCall t _ _) = t

class Cast a b where
    cast :: a -> b

instance Cast a b => Cast [a] [b] where
    cast = map cast


instance Show Type where
    show IntT        = "int"
    show (FunT as r) = toString $ "(" <> intercalate ", " (map show as) <> ") -> " <> show r
    show (ProcT xs)  = toString $ "(" <> intercalate ", " (map show xs) <> ") -> void"

deriving instance Eq Type
