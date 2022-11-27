{-# LANGUAGE TemplateHaskell #-}
module Neon.LIR (
    Def(..)
,   Body(..)
,   BasicBlock (..)
,   BasicBlockData(..)
,   Terminator(..)
,   InlineAsmComponent (..)
,   Place(..)
,   Statement(..)
,   RValue (..)
,   Operand (..)
,   Shape (..)
,   Literal (..)
,   PurePrimOp (..)

,   Local(..)
) where

import Neon.Prelude

import Neon.Pretty
import Neon.Syntax (Name) -- ugh
import Neon.Local

data Def where
    DefFunction :: {
        funName :: Name
    ,   argCount :: Int
    ,   localShapes :: Seq Shape
    ,   returnShapes :: Seq Shape
    ,   body :: Body
    } -> PrettyAnn "$0($1*)[$2*', '] -> [$3*', ']:\n    $4" Def 

data Body = Body {
        blocks :: Seq BasicBlockData
    }
instance Pretty Body where
    pretty Body { blocks } =
        intercalate "\n\n" (toList $ mapWithIndex (\i block -> "_bb" <> show i <> ":\n" <> pretty block) $ blocks)

data BasicBlockData = BasicBlockData {
    statements :: Seq Statement,
    terminator :: Terminator
}
instance Pretty BasicBlockData where
    pretty BasicBlockData {statements, terminator} =
        intercalate "\n" (toList $ fmap (("    "<>) . pretty) statements |> ("    " <> pretty terminator))

newtype BasicBlock where
    BasicBlock :: { 
        blockIndex :: Int
    } -> PrettyAnn "_bb$0" BasicBlock 
    deriving (Eq)


data Terminator where
    Goto :: BasicBlock -> (PrettyAnn "goto -> $0" Terminator)
    Return :: (PrettyAnn "return" Terminator)
    Call ::
        { callFun :: Name
        , callArgs :: Seq Operand
        -- There are no compound types in LIR. Instead LIR supports multiple returns, so
        -- instead of returning a compound type, functions just return into multiple atomic places.
        -- Note that we don't need multiple returns in Neon/MIR, since compound types are strictly more
        -- powerful than simple multi-returns like in LIR
        , destinationPlaces :: Seq Place
        , target :: BasicBlock
        } -> PrettyAnn "[$2*', '] := $0($1*', ') -> $3" Terminator
    
    -- MIR's main branching construct are `CaseNumber` terminators.
    -- In LIR these are lowered to a search tree of lookup terminators.
    -- If onLess = onGreater, code generation will only generate a single comparison
    -- for the equality case. This is especially important for if expressions,
    -- which desugar to exactly this kind of terminator.
    CompareOperand :: {
        operand    :: Operand
    ,   comparison :: Int
    ,   onLess     :: BasicBlock
    ,   onEqual    :: BasicBlock
    ,   onGreater  :: BasicBlock
     } -> PrettyAnn "if $0\n        | < $1 -> $2\n        | == $1 -> $3\n        | > $1 -> $4" Terminator

    InlineAsm :: {
        components :: Seq InlineAsmComponent
    ,   target :: BasicBlock
    } -> PrettyAnn "[asm|$0*''|] -> $1" Terminator

data InlineAsmComponent where 
    AsmText :: Text -> PrettyAnn "$0" InlineAsmComponent
    AsmOperand :: Operand -> PrettyAnn "$$0$" InlineAsmComponent

data Place where
    LocalPlace    :: Local -> PrettyAnn "$0" Place
    -- LIR supports multiple return values, so there are multiple
    -- return places.
    ReturnPlace   :: Int -> PrettyAnn "_ret_$0" Place
    WildCardPlace :: PrettyAnn "_" Place

data Statement where
    Assign :: Place -> RValue -> (PrettyAnn "$0 := $1" Statement)

data RValue where
    Use :: Operand -> PrettyAnn "$0" RValue
    PurePrimOp :: PurePrimOp -> Seq Operand -> PrettyAnn "$0#($1*', ')" RValue

data Operand where
    Literal :: Literal -> (PrettyAnn "$0" Operand)
    Copy :: Place -> (PrettyAnn "$0" Operand)

data Shape where
    IntS :: PrettyAnn "int" Shape

data Literal where
    IntLit :: Int -> (PrettyAnn "$0" Literal)
    UnitLit :: (PrettyAnn "()" Literal)

data PurePrimOp where
    PrimAdd :: PrettyAnn "+" PurePrimOp
    PrimLE :: PrettyAnn "<=" PurePrimOp

makePretty ''Def
makePretty ''BasicBlock
makePretty ''Terminator
makePretty ''Place
makePretty ''RValue
makePretty ''Statement
makePretty ''InlineAsmComponent
makePretty ''Operand
makePretty ''Shape
makePretty ''Literal
makePretty ''PurePrimOp
