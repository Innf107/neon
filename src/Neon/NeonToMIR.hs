module Neon.NeonToMIR where

import Neon.Prelude

import Neon.Syntax as Neon

import Neon.MIR as MIR

import Data.List qualified as List

import Data.IntMap qualified as IntMap

data DivergenceInfo = ReturnDivergence deriving (Show) -- TODO: Include a span here

data LowerWarning = UnreachableCode DivergenceInfo deriving (Show)

compile :: Members '[Output LowerWarning] r => [Neon.Decl Typed] -> Sem r [MIR.Def]
compile = traverse compileDecl

compileDecl :: Members '[Output LowerWarning] r => Neon.Decl Typed -> Sem r MIR.Def
compileDecl (Neon.DefFunction () funName params retTy statements retExpr) = do
    let paramTypes = map (lowerType . snd) params
    let funState =
            FunState
                { nameLocals = fromList $ zipWith (\(x, _) i -> (x, i)) params [0 ..]
                , nextLocal = length params
                , localTypes = fromList paramTypes
                , blockData = []
                }

    statements <- pure $ fromList statements |> Perform () (Neon.Return () retExpr)
    let returnType = lowerType retTy

    (state, ()) <- runState funState $ do
        block <- reserveBlock
        mlastBlock <- compileStatements block statements
        case mlastBlock of
            Nothing -> pure ()
            Just lastBlock -> void 
                $ finishBlock MIR.Return 
                $ addStatements [Assign ReturnPlace (Use (Literal MIR.UnitLit))] lastBlock
    
    let FunState { localTypes, blockData } = state

    let cleanedBlocks = blockData & mapWithIndex \i -> \case
            Nothing -> error $ "Unfinished partial block at index " <> show i
            Just bbd -> bbd


    pure $ MIR.DefFunction funName (length params) (localTypes) returnType (Body { blocks=cleanedBlocks })

compileStatements :: Members '[Output LowerWarning, State FunState] r => PartialBlockData -> Seq (Neon.Statement Typed) -> Sem r (Maybe PartialBlockData)
compileStatements currentBlock = \case 
    Empty -> pure (Just currentBlock)
    DefVar () varName expr :<| statements -> do
        local <- newLocal varName (lowerType (getType expr))
        runError (compileExprTo (LocalPlace local) currentBlock expr) >>= \case
            Left info -> do
                output (UnreachableCode info)
                pure Nothing
            Right nextBlock -> 
                compileStatements nextBlock statements
    Perform () expr :<| statements -> do
        mnextBlock <- runError $ compileExprTo WildCardPlace currentBlock expr
        case (mnextBlock, statements) of
            (Left ReturnDivergence, Empty) -> pure Nothing
            (Left info, _ :<| _) -> do
                output (UnreachableCode info)
                pure Nothing
            (Right nextBlock, _) -> compileStatements nextBlock statements
    Neon.InlineAsm () components :<| statements -> do
        mcomponents <- runError $ compileInlineAsm currentBlock components
        case mcomponents of
            Left info -> do
                output (UnreachableCode info)
                pure Nothing
            Right (components', lastAsmBlock) -> do
                continuationBlock <- reserveBlock
                let terminator = MIR.InlineAsm {
                    components = components'
                ,   target = partialBlockIndex continuationBlock
                }
                _ <- finishBlock terminator lastAsmBlock
                compileStatements continuationBlock statements
        
compileInlineAsm :: Members '[Output LowerWarning, State FunState, Error DivergenceInfo] r
                 => PartialBlockData 
                 -> Seq (Neon.InlineAsmComponent Typed) 
                 -> Sem r (Seq MIR.InlineAsmComponent, PartialBlockData)
compileInlineAsm currentBlock Empty = pure ([], currentBlock)
compileInlineAsm currentBlock (Neon.AsmText () text :<| components) = do
    let component = MIR.AsmText text
    (restComponents, finalBlock) <- compileInlineAsm currentBlock components
    pure (component <| restComponents, finalBlock)
compileInlineAsm currentBlock (Neon.AsmInterpolation () expr :<| components) = do
    local <- newAnonymousLocal (lowerType (getType expr))
    block' <- compileExprTo (LocalPlace local) currentBlock expr
    (rest, finalBlock) <- compileInlineAsm block' components
    pure (MIR.AsmOperand (Copy (LocalPlace local)) <| rest, finalBlock)

compileExprTo :: Members '[State FunState, Error DivergenceInfo, Output LowerWarning] r 
              => Place 
              -> PartialBlockData 
              -> Neon.Expr Typed 
              -> Sem r PartialBlockData
compileExprTo targetPlace currentBlock = \case
    Neon.IntLit () n -> do
        let block = addStatements [MIR.Assign targetPlace (Use (Literal (MIR.IntLit n)))] currentBlock
        pure block
    Neon.UnitLit () -> do
        let block = addStatements [MIR.Assign targetPlace (Use (Literal (MIR.UnitLit)))] currentBlock
        pure block
    Var _ty varName -> do
        (varLocal, _varType) <- localForVar varName
        let block = addStatements [MIR.Assign targetPlace (Use (Copy (LocalPlace varLocal)))] currentBlock
        pure block
    FCall _ty funName argExprs -> do
        exprsWithLocals <- traverse (\expr -> (, expr) <$> newAnonymousLocal (lowerType (getType expr))) argExprs
        block <- foldrM (\(local, expr) block -> compileExprTo (LocalPlace local) block expr) currentBlock exprsWithLocals
        nextBlock <- reserveBlock
        let terminator = Call {
                    callFun = funName
                ,   callArgs = fmap (\(local, _) -> Copy (LocalPlace local)) (fromList exprsWithLocals)
                ,   destinationPlace = targetPlace
                ,   target = (partialBlockIndex nextBlock)
                }
        finishBlock terminator block
        -- Return empty block data for the new, currently unwritten 'nextBlock'
        pure nextBlock
    BinOp _ty left Add right -> do
        leftLocal <- newAnonymousLocal MIR.IntT
        block <- compileExprTo (LocalPlace leftLocal) currentBlock left
        rightLocal <- newAnonymousLocal MIR.IntT
        block <- compileExprTo (LocalPlace rightLocal) block right
        pure $ addStatements [MIR.Assign targetPlace (PurePrimOp PrimAdd [Copy (LocalPlace (leftLocal)), Copy (LocalPlace (rightLocal))])] block
    BinOp _ty left LE right -> do
        leftLocal <- newAnonymousLocal MIR.IntT
        block <- compileExprTo (LocalPlace leftLocal) currentBlock left
        rightLocal <- newAnonymousLocal MIR.IntT
        block <- compileExprTo (LocalPlace rightLocal) block right
        pure $ addStatements [MIR.Assign targetPlace (PurePrimOp PrimLE [Copy (LocalPlace (leftLocal)), Copy (LocalPlace (rightLocal))])] block
    Neon.Return () expr -> do
        lastBlock <- compileExprTo ReturnPlace currentBlock expr
        _ <- finishBlock MIR.Return lastBlock
        throw ReturnDivergence
    Neon.ExprBlock () statements retExpr -> do
        compileStatements currentBlock statements >>= \case
            -- The block diverges in all branches, so this expression necessarily diverges
            Nothing -> throw ReturnDivergence
            Just lastBlock -> compileExprTo targetPlace lastBlock retExpr
    Neon.If _ty condition thenBranch elseBranch -> do
        condLocal <- newAnonymousLocal MIR.BoolT
        ifBlockData <- compileExprTo (LocalPlace condLocal) currentBlock condition
        
        thenBlock <- reserveBlock
        elseBlock <- reserveBlock
        
        mthenBlockData <- runError $ compileExprTo targetPlace thenBlock thenBranch
        
        melseBlockData <- runError $ compileExprTo targetPlace elseBlock elseBranch

        continuationBlock <- reserveBlock

        case mthenBlockData of
            Left ReturnDivergence -> pure ()
            Right thenBlockData -> void $ finishBlock (Goto (partialBlockIndex continuationBlock)) thenBlockData
        case melseBlockData of
            Left ReturnDivergence -> pure ()
            Right elseBlockData -> void $ finishBlock (Goto (partialBlockIndex continuationBlock)) elseBlockData

        _ <- finishBlock (CaseNumber (Copy (LocalPlace condLocal)) 
            [ (1, (partialBlockIndex thenBlock))
            , (0, (partialBlockIndex elseBlock))
            ]) ifBlockData
        pure continuationBlock

newAnonymousLocal :: Members '[State FunState] r => MIR.Type -> Sem r Local
newAnonymousLocal ty = state (\s@FunState{nextLocal, localTypes} -> 
    ( Local { localIx = nextLocal, localName = Nothing }
    , s { nextLocal = nextLocal + 1
        , localTypes = localTypes |> ty
        }))

newLocal :: Members '[State FunState] r => Name -> MIR.Type -> Sem r Local
newLocal name ty = state (\s@FunState{nameLocals, nextLocal, localTypes} -> 
    ( Local { localIx = nextLocal, localName = Just name }
    , s { nameLocals = insert name nextLocal nameLocals
        , nextLocal = nextLocal + 1
        , localTypes = localTypes |> ty
        }))

localForVar :: Members '[State FunState] r => Name -> Sem r (Local, MIR.Type)
localForVar name = do
    FunState { nameLocals, localTypes } <- get
    let local = case lookup name nameLocals of
            Just local -> local
            Nothing -> error $ "NeonToMIR.localForVar: Invalid local variable '" <> show name <> "'. There is no local associated with this variable!"
    let ty = index localTypes local
    pure (Local { localIx = local, localName = Just name }, ty)

finishBlock :: Members '[State FunState] r 
            => Terminator 
            -> PartialBlockData 
            -> Sem r BasicBlock
finishBlock terminator PartialBlockData { partialStatements, partialBlockIndex } = do
    let BasicBlock{ blockIndex } = partialBlockIndex
    
    let newBBData = BasicBlockData {
            statements = partialStatements
        ,   terminator
        }
    state \s@FunState { blockData } -> 
        ( partialBlockIndex
        , s { blockData = insertAt blockIndex (Just newBBData) $ deleteAt blockIndex $ blockData })
        
        

reserveBlock :: Members '[State FunState] r => Sem r PartialBlockData
reserveBlock = do
    nextIndex <- state (\s@FunState{blockData} -> (length blockData, s{blockData = blockData |> Nothing}))
    pure $ PartialBlockData {
        partialStatements = []
    ,   partialBlockIndex = BasicBlock { blockIndex = nextIndex }
    }

data FunState = FunState
    { nameLocals :: Map Name Int
    , nextLocal :: Int
    , localTypes :: Seq MIR.Type
    , blockData :: Seq (Maybe BasicBlockData)
    }

lowerType :: Neon.Type -> MIR.Type
lowerType Neon.IntT = MIR.IntT
lowerType Neon.BoolT = MIR.BoolT
lowerType Neon.UnitT = MIR.UnitT
lowerType Neon.NeverT = MIR.NeverT
