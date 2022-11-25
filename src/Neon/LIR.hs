module Neon.LIR where

import Neon.Prelude

import Neon.Pretty
import Neon.Syntax (Name) -- ugh

data Def where
    DefFunction :: {
        funName :: Name
    ,   argCount :: Int
    ,   localShapes :: Seq Shape
    ,   returnShape :: Shape
    ,   body :: Body
    } -> PrettyAnn "$1($2*', ')[$3*', '] -> $4:\n    $5" Def 


data Shape where
    IntS :: PrettyAnn "int" Shape
