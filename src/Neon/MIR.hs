{-# LANGUAGE TemplateHaskell #-}
module Neon.MIR (
    Def (..),
    Body (..),
    BasicBlock (..),
    BasicBlockData (..),
    Statement (..),
    InlineAsmComponent (..),
    Terminator (..),
    Place (..),
    RValue (..),
    Operand (..),
    Literal (..),
    PurePrimOp (..),
    Type (..),
    Local (..),

    PartialBlockData (..),
    addStatements,
) where

import Neon.Prelude

import Neon.Local

import Neon.Util
import Neon.Pretty

import Neon.Syntax (Name, renderNameNoPrefix, renderName) -- Ugh
import Data.Unique
import Data.IntMap qualified as IntMap
import Neon.Config (Config(..), getConfig)

class MakePretty a

data Type = IntT 
          | BoolT
          | UnitT
          | NeverT
          deriving (Show, Eq)

instance Pretty Type where
    pretty IntT = "int"
    pretty BoolT = "bool"
    pretty UnitT = "()"
    pretty NeverT = "!"

-- Definitions store the types of parameters *and all locals*!
-- Because of this, we need to store the number of parameters separately
data Def = DefFunction (PrettyAnn "fn $0($1) [$2*' '] -> $3:\n$4" Name) Int (Seq Type) Type Body

instance Pretty [Def] where
    pretty defs = intercalate "\n\n" (map pretty defs)

data Body = Body {
    blocks :: Seq BasicBlockData
}

instance Pretty Body where
    pretty Body {blocks} =
        intercalate "\n\n" (toList $ mapWithIndex (\i block -> "_bb" <> show i <> ":\n" <> pretty block) $ blocks)

-- A block is represented as an index into the function's block map.
-- This makes it possible to compare blocks for 'reference' equality
newtype BasicBlock where
    BasicBlock :: { 
        blockIndex :: Int
    } -> PrettyAnn "_bb$0" BasicBlock 
    deriving (Eq)
instance MakePretty BasicBlock

data BasicBlockData = BasicBlockData {
    statements :: Seq Statement,
    terminator :: Terminator
}
instance Pretty BasicBlockData where
    pretty BasicBlockData {statements, terminator} =
        intercalate "\n" (toList $ fmap (("    "<>) . pretty) statements |> ("    " <> pretty terminator))

data PartialBlockData = PartialBlockData {
    partialStatements :: Seq Statement
,   partialBlockIndex :: BasicBlock
}

addStatements :: Seq Statement -> PartialBlockData -> PartialBlockData
addStatements statements (blockData@PartialBlockData { partialStatements }) = 
    blockData { partialStatements = partialStatements <> statements}



data Terminator where
    -- | This block has a single successor. Execution directly continues there
    Goto :: BasicBlock -> (PrettyAnn "goto -> $0" Terminator)
    -- | "I'll be back"
    Return :: (PrettyAnn "return" Terminator)
    -- | Call a function (currently represented by its name due to the lack of first-class functions) 
    -- with a 'target' block to continue in with the result.
    Call ::
        { callFun :: Name
        , callArgs :: Seq Operand
        , destinationPlace :: Place
        , target :: BasicBlock
        } -> PrettyAnn "$2 := $0($1*', ') -> $3" Terminator
    CaseNumber :: Operand -> Seq (Int, BasicBlock) -> PrettyAnn (PrettyVia "prettyCaseNumber") Terminator

    -- Inline assembly might perform any arbitrarily complex control flow,
    -- so we need to treat it as a terminator (TODO: Check if rustc does this as well)
    InlineAsm :: {
        components :: Seq InlineAsmComponent
    ,   target :: BasicBlock
    } -> PrettyAnn "[asm|$0*''|] -> $1" Terminator

prettyCaseNumber :: Operand -> Seq (Int, BasicBlock) -> Text
prettyCaseNumber operand branches = "case " <> pretty operand <> " {" 
    <> foldMap (\(n, block) -> "\n        " <> pretty n <> " -> " <> pretty block) branches
    <> "\n    }"

data InlineAsmComponent where 
    AsmText :: Text -> PrettyAnn "$0" InlineAsmComponent
    AsmOperand :: Operand -> PrettyAnn "$$0$" InlineAsmComponent
    



data Place where
    LocalPlace    :: Local -> (PrettyAnn "$0" Place)
    ReturnPlace   :: PrettyAnn "_ret" Place
    WildCardPlace :: PrettyAnn "_" Place

data Statement where
    Assign :: Place -> RValue -> (PrettyAnn "$0 := $1" Statement)

data RValue where
    Use :: Operand -> PrettyAnn "$0" RValue
    PurePrimOp :: PurePrimOp -> Seq Operand -> PrettyAnn "$0#($1*', ')" RValue

data Operand where
    Literal :: Literal -> (PrettyAnn "$0" Operand)
    Copy :: Place -> (PrettyAnn "$0" Operand)

data Literal where
    IntLit :: Int -> (PrettyAnn "$0" Literal)
    UnitLit :: (PrettyAnn "()" Literal)

data PurePrimOp where
    PrimAdd :: PrettyAnn "+" PurePrimOp
    PrimLE :: PrettyAnn "<=" PurePrimOp

makePretty ''Def
makePretty ''Terminator
makePretty ''Place
makePretty ''RValue
makePretty ''Statement
makePretty ''InlineAsmComponent
makePretty ''Operand
makePretty ''Literal
makePretty ''PurePrimOp


forAllInstances ''MakePretty makePretty
