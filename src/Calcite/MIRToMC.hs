module Calcite.MIRToMC where

import Calcite.Prelude
import Calcite.Types.AST (Name (..))

import Calcite.MIR as MIR

import Data.Map qualified as Map
import Data.IntMap qualified as IntMap
import Data.List qualified as List

compile :: [Def] -> Sem r [(FilePath, Text)]
compile defs = do
    let initialLowerState = LowerState {
            funs = mempty
        }
    (state, _) <- runState initialLowerState $ traverse compileDef defs
    let LowerState { funs } = state
    pure $ Map.toList funs
    
{- TODO: Do some analysis and try to inline terminators with out-degree 1 such as `goto` and `call`.
   To do this, we need to be sure that we don't massively duplicate code.

    For example:
    _bb0:
      if something
      then goto _bb1
      else goto _bb2
    _bb1:
      goto _bb3
    _bb2:
      goto _bb3
    _bb3:
      <a ton of statements>

    If we were to naively inline both `goto _bb3` terminators, we would duplicate the massive _bb3 block,
    which would generate way too much code. 
    We would need to make sure that either _bb3 is small enough, that inlining is fine either way 
    or that _bb3 only has a single inbound edge.
-}

compileDef :: Members '[State LowerState] r => Def -> Sem r ()
compileDef = \case 
    DefFunction funName argCount localShapes returnShape body -> do
        let funInfo = FunctionInfo { funName, argCount, localShapes, returnShape }
        let Body { blocks } = body
        runReader funInfo 
            $ traverse_ (\(blockIndex, u) -> compileBlock (BasicBlock {blockIndex}) u) 
            $ IntMap.toList blocks

compileBlock :: Members '[State LowerState, Reader FunctionInfo] r 
             => BasicBlock
             -> BasicBlockData 
             -> Sem r ()
compileBlock block blockData = do
    path <- blockFunPath False block
    let partialMCFun = PartialMCFun { path, commands = [] }
    let BasicBlockData { statements, terminator } = blockData 

    (partialFun, _) <- runState partialMCFun $ traverse_ compileStatement statements
    PartialMCFun {path, commands} <- compileTerminator partialFun terminator
    modify (\s@LowerState{ funs } -> s{funs = insert path (unlines (toList commands)) funs })

compileStatement :: Members '[State LowerState, Reader FunctionInfo, State PartialMCFun] r
                 => Statement
                 -> Sem r ()
compileStatement = \case 
    Assign place rvalue -> compileAssign place rvalue

compileAssign :: Members '[State LowerState, Reader FunctionInfo, State PartialMCFun] r 
              => Place 
              -> RValue 
              -> Sem r ()
compileAssign place rvalue = case place of
    VarPlace local -> do
        shape <- localShape local        
        case shape of
            Number -> do
                score <- localToScore local
                assignToScore score rvalue
    ReturnPlace -> do
        shape <- asks returnShape
        case shape of
            Number -> do
                score <- returnScore
                assignToScore score rvalue
    WildCardPlace -> do
        -- We don't need to compile assignments to wildcards, since all rvalues are pure.
        -- Ideally, these should be optimized out before lowering anyway.
        pure ()

assignToScore :: Members '[State LowerState, Reader FunctionInfo, State PartialMCFun] r
              => Text 
              -> RValue 
              -> Sem r () 
assignToScore score rvalue = do
    case rvalue of
        Use (Literal lit) -> case lit of
            IntLit n -> emitCommands ["scoreboard players set " <> score <> " calcite " <> show n]
            UnitLit -> undefined
        Use (Copy place) -> case place of
            ReturnPlace -> error $ "MIRToMC.assignToScore: Trying to assign from return place to numeric score '" <> score <> "'"
            WildCardPlace -> error $ "MIRToMC.assignToScore: Trying to assign from wildcard place to numeric score '" <> score <> "'"
            VarPlace local -> do
                localScore <- localToScore local
                emitCommands ["scoreboard players operation " <> score <> " calcite = " <> localScore <> " calcite"]
        BinOp op bo op' -> undefined
    

compileTerminator :: forall r. Members '[State LowerState, Reader FunctionInfo] r => PartialMCFun -> Terminator -> Sem r PartialMCFun
compileTerminator partialFun = \case
    Goto bb -> do
        blockPath <- blockFunPath True bb
        pure $ addCommands ["call " <> toText blockPath] partialFun
    Return -> pure partialFun -- Returns are implicit in mcfunctions
    Call funName args returnPlace continuationBlock -> do
        -- Args are passed as the first n locals
        (partialFun, _) <- runState partialFun $ forM_ @_ @(Sem (State PartialMCFun : r)) (List.zip [0..] (toList args)) \(i, operand) -> do
            -- TODO: Do something about other shapes and get the relevant shape in the first place
            case operand of
                Literal (IntLit n) -> 
                    emitCommands ["scoreboard players set " <> localInFun i funName <> " calcite " <> show n]
                Literal UnitLit -> undefined
                Copy place -> case place of
                    VarPlace local -> do
                        localScore <- localToScore local 
                        emitCommands ["scoreboard players operation " <> localInFun i funName <> " calcite = " <> localScore <> " calcite"]
                    ReturnPlace -> error $ "MIRToMC.compileTerminator: Trying to assign from return place"
                    WildCardPlace -> error $ "MIRToMC.compileTerminator: Trying to assign from wildcard place"
        
        
        partialFun <- pure (addCommands ["call " <> show funName] partialFun)

        -- We need to copy the returned value to `returnPlace`
        partialFun <- asks returnShape >>= \case 
            -- TODO: Would be nice to avoid some of the duplication between this and compileAssign
            Number -> do
                case returnPlace of
                    VarPlace n -> do
                        score <- localToScore n
                        pure (addCommands ["scoreboard players operation " <> score <> " calcite = " <> returnScoreForFun funName <> " calcite"] partialFun)
                    ReturnPlace -> do
                        score <- returnScore
                        pure (addCommands ["scoreboard players operation " <> score <> " calcite = " <> returnScoreForFun funName <> " calcite"] partialFun)
                    WildCardPlace -> pure partialFun
        
            

        continuationPath <- blockFunPath True continuationBlock
        pure $ addCommands ["call " <> toText continuationPath] partialFun


blockFunPath :: Members '[Reader FunctionInfo] r => Bool -> BasicBlock -> Sem r FilePath
blockFunPath includePrefix (BasicBlock { blockIndex }) = do
    FunctionInfo { funName } <- ask
    let Name {originalName, nameIndex } = funName
    let funPrefix = 
            if includePrefix then 
                show funName
            else
                if nameIndex == 0 then 
                    originalName
                else 
                    originalName <> "_" <> show nameIndex
    if blockIndex == 0 then
        pure $ toString funPrefix
    else
        pure $ toString $ funPrefix <> "_bb" <> show nameIndex

data PartialMCFun = PartialMCFun {
    path :: FilePath
,   commands :: Seq Text
}

addCommands :: Seq Text -> PartialMCFun -> PartialMCFun
addCommands newCommands (PartialMCFun { path, commands }) = 
    PartialMCFun {
        path
    ,   commands = commands <> newCommands
    } 

emitCommands :: Members '[State PartialMCFun] r => Seq Text -> Sem r ()
emitCommands commands = modify (addCommands commands)

newtype LowerState = LowerState {
    funs :: Map FilePath Text
}

data FunctionInfo = FunctionInfo {
        funName :: Name
    ,   argCount :: Int
    ,   localShapes :: Seq Shape
    ,   returnShape :: Shape
    }

localShape :: Members '[Reader FunctionInfo] r => Int -> Sem r Shape
localShape i = asks (\FunctionInfo { localShapes } -> index localShapes i)

localInFun :: Int -> Name -> Text
localInFun local funName = show funName <> "-" <> show local

localToScore :: Members '[Reader FunctionInfo] r => Int -> Sem r Text
localToScore local = do
    FunctionInfo { funName } <- ask
    pure (localInFun local funName)

returnScoreForFun :: Name -> Text
returnScoreForFun funName = show funName <> "-ret"

returnScore :: Members '[Reader FunctionInfo] r => Sem r Text
returnScore = do
    FunctionInfo { funName } <- ask
    pure (returnScoreForFun funName)

