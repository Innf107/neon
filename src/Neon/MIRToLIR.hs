module Neon.MIRToLIR where

import Neon.Prelude

import Neon.MIR as MIR
import Neon.LIR as LIR

import Data.Sequence qualified as Seq

compile :: [MIR.Def] -> Sem r [LIR.Def]
compile = traverse compileDef

compileDef :: MIR.Def -> Sem r LIR.Def
compileDef = \case 
    MIR.DefFunction funName argCount localTypes returnType body -> do 
        localShapes <- fold <$> traverse shapesForType localTypes
        retrunShapes <- shapesForType returnType

        body' <- compileBody body

        pure (LIR.DefFunction funName argCount localShapes retrunShapes body')

compileBody :: MIR.Body -> Sem r (LIR.Body)
compileBody (MIR.Body blockData) = LIR.Body <$> traverse compileBlock blockData

compileBlock :: MIR.BasicBlockData -> Sem r (LIR.BasicBlockData)
compileBlock MIR.BasicBlockData { statements, terminator } = do
    LIR.BasicBlockData 
        <$> (fold <$> traverse compileStatement statements)
        <*> compileTerminator terminator 

compileStatement :: MIR.Statement -> Sem r (Seq LIR.Statement)
compileStatement = \case 
    MIR.Assign place rvalue -> do
        shapes <- shapesForType =<< rvalueType rvalue

        places <- lowerAndSplitPlace shapes place
        rvalues <- lowerAndSplitRValue rvalue

        assertM (length places == length rvalues)
        pure $ Seq.zipWith (\place rvalue -> LIR.Assign place rvalue) places rvalues

rvalueType :: MIR.RValue -> Sem r (MIR.Type)
rvalueType = \case
    MIR.Use (MIR.Copy place) -> undefined
    MIR.Use (MIR.Literal lit) -> case lit of
        MIR.IntLit _ -> pure IntT
        MIR.UnitLit -> pure UnitT
    MIR.PurePrimOp primOp _ -> case primOp of 
        MIR.PrimAdd -> pure IntT
        MIR.PrimLE -> pure BoolT

lowerAndSplitPlace :: Seq LIR.Shape -> MIR.Place -> Sem r (Seq LIR.Place)
lowerAndSplitPlace shapes = \case
    MIR.LocalPlace local -> undefined
    MIR.ReturnPlace -> pure $ mapWithIndex (\i _ -> LIR.ReturnPlace i) shapes
    MIR.WildCardPlace -> pure $ fmap (\_ -> LIR.WildCardPlace) shapes
    

lowerAndSplitRValue :: MIR.RValue -> Sem r (Seq LIR.RValue)
lowerAndSplitRValue = \case
    MIR.Use op -> undefined
    MIR.PurePrimOp ppo seq -> undefined

compileTerminator :: MIR.Terminator -> Sem r (LIR.Terminator)
compileTerminator = undefined

shapesForType :: MIR.Type -> Sem r (Seq LIR.Shape)
shapesForType = \case
    IntT    -> pure [IntS]
    BoolT   -> pure [IntS]
    UnitT   -> pure [IntS] -- TODO: This should really be zero sized
    NeverT  -> pure [IntS] -- Not sure what to do here. This should probably be zero sized, since it can never be constructed?

