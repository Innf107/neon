module Calcite.MIRToMC where

import Calcite.Prelude
import Calcite.Types.AST (Name (..))

import Calcite.MIR as MIR

compile :: [Def] -> [(FilePath, Text)]
compile = undefined