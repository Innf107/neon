{-# LANGUAGE TemplateHaskell #-}
module Calcite.MIR (
    Def (..),
    Body (..),
    BasicBlock (..),
    BasicBlockData (..),
    Statement (..),
    Terminator (..),
    Place (..),
    RValue (..),
    Operand (..),
    Literal (..),
    BinOp (..),
    Shape (..),

    PartialBlockData (..),
    emptyBlockData,
    addStatements,
    finishBlock,
) where

import Calcite.Prelude

import Calcite.Pretty

import Calcite.Types.AST (Name) -- Ugh
import Data.Unique
import Data.IntMap qualified as IntMap

{- | The Shape is effectively the 'type' of LIR expressions.
 This is not a traditional nominal type, but more about the, well, shape
 of data used for compilation. This will also be used for monomorphization,
 similar to go's 'GC shapes' once we have polymorphism.
-}
data Shape = Number deriving (Show, Eq)

instance Pretty Shape where
    pretty Number = "N"

-- Definitions store the shapes of parameters *and all locals*!
-- Because of this, we need to store the number of parameters separately
data Def = DefFunction (PrettyAnn "fn $0($1) [$2*' ']:\n$3" Name) Int (Seq Shape) Body

instance Pretty [Def] where
    pretty defs = intercalate "\n\n" (map pretty defs)

data Body = Body {
    blocks :: IntMap BasicBlockData
}

instance Pretty Body where
    pretty Body {blocks} =
        intercalate "\n\n" (fmap (\(i, block) -> "_bb" <> show i <> ":\n" <> pretty block) $ IntMap.toList blocks)

-- A block is represented as an index into the function's block map.
-- This makes it possible to compare blocks for 'reference' equality
newtype BasicBlock = BasicBlock { blockIndex :: (PrettyAnn "_bb$0" Int) } deriving (Eq)

data BasicBlockData = BasicBlockData {
    statements :: Seq Statement,
    terminator :: Terminator
}

data PartialBlockData = PartialBlockData {
    partialStatements :: Seq Statement
}

addStatements :: Seq Statement -> PartialBlockData -> PartialBlockData
addStatements statements (blockData@PartialBlockData { partialStatements }) = 
    blockData { partialStatements = partialStatements <> statements}

emptyBlockData :: PartialBlockData
emptyBlockData = PartialBlockData {
    partialStatements = []
}

finishBlock :: Terminator -> PartialBlockData -> BasicBlockData
finishBlock terminator PartialBlockData { partialStatements } = BasicBlockData {
        statements = partialStatements
    ,   terminator
    }

instance Pretty BasicBlockData where
    pretty BasicBlockData {statements, terminator} =
        intercalate "\n" (toList $ fmap (("    "<>) . pretty) statements)
        <> "\n    " <> pretty terminator

data Terminator
    -- | This block has a single successor. Execution directly continues there
    = Goto (PrettyAnn "goto -> $0" BasicBlock)
    | Return (PrettyAnn "return" ())
    -- | Call a function (currently represented by its name due to the lack of first-class functions) 
    -- with a 'target' block to continue in with the result.
    | Call 
        { callFun :: (PrettyAnn "$2 := $0($1*', ') -> $3" Name) 
        , callArgs :: (Seq Operand) 
        , destinationPlace :: Place
        , target :: BasicBlock
        }

-- In LIR, expressions are only simple pure operations and all non-trivial / expensive
-- computations happen in statements.

data Place
    = VarPlace (PrettyAnn "_$0" Int)
    | ReturnPlace (PrettyAnn "_ret" ())
    | WildCardPlace (PrettyAnn "_" ())

data Statement
    = Assign (PrettyAnn "$0 := $1" Place) RValue

data RValue = Use (PrettyAnn "$0" Operand)
            | BinOp (PrettyAnn "$0 $1 $2" Operand) BinOp Operand

data Operand 
    = Literal (PrettyAnn "$0" Literal)
    | Copy (PrettyAnn "$0" Place)

data Literal = IntLit (PrettyAnn "$0" Int)
             | UnitLit (PrettyAnn "()" ())

data BinOp = Add

instance Pretty BinOp where
    pretty Add = "+"


makePretty ''Def
makePretty ''BasicBlock
makePretty ''Terminator
makePretty ''Place
makePretty ''RValue
makePretty ''Statement
makePretty ''Operand
makePretty ''Literal


