{-# LANGUAGE TemplateHaskell #-}
module Neon.MIR (
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
    PurePrimOp (..),
    Shape (..),
    Local (..),

    PartialBlockData (..),
    emptyBlockData,
    addStatements,
    finishBlock,
) where

import Neon.Prelude

import Neon.Pretty

import Neon.Syntax (Name, renderNameNoPrefix, renderName) -- Ugh
import Data.Unique
import Data.IntMap qualified as IntMap
import Neon.Config (Config(..), getConfig)

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


data Local = Local {
    localIx :: Int
    -- localName is 'Just somename' iff this local was derived from a user-written variable.
    -- IMPORTANT: localName exists exclusively for debug information. Two locals with the same
    -- ix but different names will always behave the same way (except when displayed).
,   localName :: Maybe Name
}
instance Eq Local where (==) = (==) `on` localIx
instance Ord Local where compare = compare `on` localIx
instance Pretty Local where
    pretty (Local ix Nothing) = "_" <> show ix
    pretty (Local ix (Just name)) =
        let Config { printLocalPrefix } = getConfig () in
        if printLocalPrefix then
            renderName name <> "_" <> show ix
        else
            renderNameNoPrefix name <> "_" <> show ix


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

makePretty ''Def
makePretty ''BasicBlock
makePretty ''Terminator
makePretty ''Place
makePretty ''RValue
makePretty ''Statement
makePretty ''Operand
makePretty ''Literal
makePretty ''PurePrimOp


