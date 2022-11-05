module Calcite.CalciteToMIR where

import Calcite.Prelude

import Calcite.Types.AST as C

import Calcite.MIR as MIR

import Data.List qualified as List

compile :: [C.Decl Typed] -> Sem r [MIR.Def]
compile = traverse compileDecl

compileDecl :: C.Decl Typed -> Sem r MIR.Def
compileDecl (C.DefFunction () funName params statements mreturn) = do
    let paramShapes = map (shapeForType . snd) params
    let funState =
            FunState
                { nameLocals = fromList $ zipWith (\(x, _) i -> (x, i)) params [0 ..]
                , nextLocal = length params
                , localShapes = fromList paramShapes
                , blockData = []
                }

    let statements' = case mreturn of
            Nothing -> fromList statements
            Just (retExp, _) -> fromList statements |> Perform () retExp

    (state, _) <- runState funState $ compileStatements emptyBlockData statements'

    pure $ MIR.DefFunction funName (length params) (undefined :: Seq Shape) (undefined :: Body)

compileStatements :: Members '[State FunState] r => PartialBlockData -> Seq (C.Statement Typed) -> Sem r ()
compileStatements currentBlock = \case 
    Empty -> do
        _ <- addBlock $ finishBlock (MIR.Return ()) $ addStatements [Assign (ReturnPlace ()) undefined] currentBlock
        pure ()
    DefVar () name expr :<| statements -> undefined
    Perform () expr :<| statements -> undefined
    

newLocal :: Members '[State FunState] r => Name -> Shape -> Sem r Int
newLocal name shape = state (\s@FunState{nameLocals, nextLocal, localShapes} -> 
    ( nextLocal
    , s { nameLocals = insert name nextLocal nameLocals
        , nextLocal = nextLocal + 1
        , localShapes = localShapes |> shape
        }))

addBlock :: Members '[State FunState] r => BasicBlockData -> Sem r BasicBlock
addBlock newBlock = state (\s@FunState { blockData } -> 
    (BasicBlock { blockIndex = length blockData }, s { blockData = blockData |> newBlock }))

data FunState = FunState
    { nameLocals :: Map Name Int
    , nextLocal :: Int
    , localShapes :: Seq Shape
    , blockData :: Seq BasicBlockData
    }

shapeForType :: Type -> Shape
shapeForType IntT = Number -- We only support numbers for now
shapeForType (FunT _ _) = Number -- TODO: I honestly don't think these should be *types*, since locals can never have these
shapeForType (ProcT _) = Number --  ^
shapeForType NeverT = Number -- TODO: This should probably be zero sized? It's never going to be used anyway. Maybe we should just have a never shape?
