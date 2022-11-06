module Calcite.MIRToMC where

import Calcite.Prelude
import Calcite.Types.AST (Name (..))

import Calcite.MIR as MIR

import Data.Map qualified as Map

compile :: [Def] -> Sem r [(FilePath, Text)]
compile defs = do
    let initialLowerState = LowerState {
            funs = mempty
        }
    (state, _) <- runState initialLowerState $ traverse compileDef defs
    let LowerState { funs } = state
    pure $ Map.toList funs
    
compileDef :: Def -> Sem r ()
compileDef = \case 
    DefFunction _ n seq sh bo -> undefined


data LowerState = LowerState {
    funs :: Map FilePath Text
}
