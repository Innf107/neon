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
data Def = DefFunction (PrettyAnn "fn $0($1) [$2*' '] -> $3:\n$4" Name) Int (Seq Shape) Shape Body

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
newtype BasicBlock where
    BasicBlock :: { 
        blockIndex :: Int
    } -> PrettyAnn "_bb$0" BasicBlock 
    deriving (Eq)

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
        intercalate "\n" (toList $ fmap (("    "<>) . pretty) statements |> ("    " <> pretty terminator))

data Terminator where
    -- | This block has a single successor. Execution directly continues there
    Goto :: BasicBlock -> (PrettyAnn "goto -> $0" Terminator)
    Return :: (PrettyAnn "return" Terminator)
    -- | Call a function (currently represented by its name due to the lack of first-class functions) 
    -- with a 'target' block to continue in with the result.
    Call ::
        { callFun :: Name
        , callArgs :: (Seq Operand) 
        , destinationPlace :: Place
        , target :: BasicBlock
        } -> PrettyAnn "$2 := $0($1*', ') -> $3" Terminator

-- In LIR, expressions are only simple pure operations and all non-trivial / expensive
-- computations happen in statements.

data Place where
    VarPlace      :: Int -> (PrettyAnn "_$0" Place)
    ReturnPlace   :: PrettyAnn "_ret" Place
    WildCardPlace :: PrettyAnn "_" Place

data Statement where
    Assign :: Place -> RValue -> (PrettyAnn "$0 := $1" Statement)

data RValue where
    Use :: Operand -> (PrettyAnn "$0" RValue)
    BinOp :: Operand -> BinOp -> Operand -> (PrettyAnn "$0 $1 $2" RValue)

data Operand where
    Literal :: Literal -> (PrettyAnn "$0" Operand)
    Copy :: Place -> (PrettyAnn "$0" Operand)

data Literal where
    IntLit :: Int -> (PrettyAnn "$0" Literal)
    UnitLit :: (PrettyAnn "()" Literal)

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


