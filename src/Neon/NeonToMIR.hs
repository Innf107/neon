module Neon.NeonToMIR where

import Neon.Prelude

import Neon.Syntax as C

import Neon.MIR as MIR

import Data.List qualified as List

import Data.IntMap qualified as IntMap

data DivergenceInfo = ReturnDivergence deriving (Show) -- TODO: Include a span here

data LowerWarning = UnreachableCode DivergenceInfo deriving (Show)

compile :: Members '[Output LowerWarning] r => [C.Decl Typed] -> Sem r [MIR.Def]
compile = traverse compileDecl

compileDecl :: Members '[Output LowerWarning] r => C.Decl Typed -> Sem r MIR.Def
compileDecl (C.DefFunction () funName params retTy statements retExpr) = do
    let paramShapes = map (shapeForType . snd) params
    let funState =
            FunState
                { nameLocals = fromList $ zipWith (\(x, _) i -> (x, i)) params [0 ..]
                , nextLocal = length params
                , localShapes = fromList paramShapes
                , blockData = []
                }

    statements <- pure $ fromList statements |> Perform () (C.Return () retExpr)
    let returnShape = shapeForType retTy

    (state, ()) <- runState funState $ do
        block <- reserveBlock
        mlastBlock <- compileStatements block statements
        case mlastBlock of
            Nothing -> pure ()
            Just lastBlock -> void 
                $ finishBlock MIR.Return 
                $ addStatements [Assign ReturnPlace (Use (Literal MIR.UnitLit))] lastBlock
    
    let FunState { localShapes, blockData } = state

    let cleanedBlocks = blockData & mapWithIndex \i -> \case
            Nothing -> error $ "Unfinished partial block at index " <> show i
            Just bbd -> bbd


    pure $ MIR.DefFunction funName (length params) (localShapes) returnShape (Body { blocks=cleanedBlocks })

compileStatements :: Members '[Output LowerWarning, State FunState] r => PartialBlockData -> Seq (C.Statement Typed) -> Sem r (Maybe PartialBlockData)
compileStatements currentBlock = \case 
    Empty -> pure (Just currentBlock)
    DefVar () varName expr :<| statements -> do
        local <- newLocal varName (shapeForType (getType expr))
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
    C.InlineAsm () components :<| statements -> do
        (components', lastAsmBlock) <- compileInlineAsm currentBlock components
        continuationBlock <- reserveBlock
        let terminator = MIR.InlineAsm {
            components = components'
        ,   target = partialBlockIndex continuationBlock
        }
        _ <- finishBlock terminator lastAsmBlock
        compileStatements continuationBlock statements
        
compileInlineAsm :: Members '[Output LowerWarning, State FunState] r
                 => PartialBlockData 
                 -> Seq (C.InlineAsmComponent Typed) 
                 -> Sem r (Seq MIR.InlineAsmComponent, PartialBlockData)
compileInlineAsm currentBlock Empty = pure ([], currentBlock)
compileInlineAsm currentBlock (C.AsmText () text :<| components) = do
    let component = MIR.AsmText text
    (restComponents, finalBlock) <- compileInlineAsm currentBlock components
    pure (component <| restComponents, finalBlock)
compileInlineAsm currentBlock (C.AsmInterpolation () expr :<| components) =
    undefined

compileExprTo :: Members '[State FunState, Error DivergenceInfo, Output LowerWarning] r 
              => Place 
              -> PartialBlockData 
              -> C.Expr Typed 
              -> Sem r PartialBlockData
compileExprTo targetPlace currentBlock = \case
    C.IntLit () n -> do
        let block = addStatements [MIR.Assign targetPlace (Use (Literal (MIR.IntLit n)))] currentBlock
        pure block
    C.UnitLit () -> do
        let block = addStatements [MIR.Assign targetPlace (Use (Literal (MIR.UnitLit)))] currentBlock
        pure block
    Var _ty varName -> do
        (varLocal, _varShape) <- localForVar varName
        let block = addStatements [MIR.Assign targetPlace (Use (Copy (LocalPlace varLocal)))] currentBlock
        pure block
    FCall _ty funName argExprs -> do
        exprsWithLocals <- traverse (\expr -> (, expr) <$> newAnonymousLocal (shapeForType (getType expr))) argExprs
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
        leftLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace leftLocal) currentBlock left
        rightLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace rightLocal) block right
        pure $ addStatements [MIR.Assign targetPlace (PurePrimOp PrimAdd [Copy (LocalPlace (leftLocal)), Copy (LocalPlace (rightLocal))])] block
    BinOp _ty left LE right -> do
        leftLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace leftLocal) currentBlock left
        rightLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace rightLocal) block right
        pure $ addStatements [MIR.Assign targetPlace (PurePrimOp PrimLE [Copy (LocalPlace (leftLocal)), Copy (LocalPlace (rightLocal))])] block
    C.Return () expr -> do
        lastBlock <- compileExprTo ReturnPlace currentBlock expr
        _ <- finishBlock MIR.Return lastBlock
        throw ReturnDivergence
    C.ExprBlock () statements retExpr -> do
        compileStatements currentBlock statements >>= \case
            -- The block diverges in all branches, so this expression necessarily diverges
            Nothing -> throw ReturnDivergence
            Just lastBlock -> compileExprTo targetPlace lastBlock retExpr
    C.If _ty condition thenBranch elseBranch -> do
        condLocal <- newAnonymousLocal Number
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

newAnonymousLocal :: Members '[State FunState] r => Shape -> Sem r Local
newAnonymousLocal shape = state (\s@FunState{nextLocal, localShapes} -> 
    ( Local { localIx = nextLocal, localName = Nothing }
    , s { nextLocal = nextLocal + 1
        , localShapes = localShapes |> shape
        }))

newLocal :: Members '[State FunState] r => Name -> Shape -> Sem r Local
newLocal name shape = state (\s@FunState{nameLocals, nextLocal, localShapes} -> 
    ( Local { localIx = nextLocal, localName = Just name }
    , s { nameLocals = insert name nextLocal nameLocals
        , nextLocal = nextLocal + 1
        , localShapes = localShapes |> shape
        }))

localForVar :: Members '[State FunState] r => Name -> Sem r (Local, Shape)
localForVar name = do
    FunState { nameLocals, localShapes } <- get
    let local = case lookup name nameLocals of
            Just local -> local
            Nothing -> error $ "NeonToMIR.localForVar: Invalid local variable '" <> show name <> "'. There is no local associated with this variable!"
    let shape = index localShapes local
    pure (Local { localIx = local, localName = Just name }, shape)

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
    , localShapes :: Seq Shape
    , blockData :: Seq (Maybe BasicBlockData)
    }

shapeForType :: Type -> Shape
shapeForType IntT = Number
shapeForType BoolT = Number -- Booleans are desugared into numeric shapes. Maybe we should include some information in mir, so that we know checking for 0 and 1 is exhaustive?
shapeForType UnitT = Number -- TODO: This should be something zero sized, ideally something like `TupleShape []`
shapeForType NeverT = Number -- TODO: This should probably be zero sized? It's never going to be used anyway. Maybe we should just have a never shape?
