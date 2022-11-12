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
        mlastBlock <- compileStatements emptyBlockData statements
        case mlastBlock of
            Nothing -> pure ()
            Just lastBlock -> void $ addBlock 
                $ finishBlock MIR.Return 
                $ addStatements [Assign ReturnPlace (Use (Literal MIR.UnitLit))] lastBlock
    
    let FunState { localShapes, blockData } = state

    pure $ MIR.DefFunction funName (length params) (localShapes) returnShape (Body { blocks=blockData })

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
            (Left ReturnDivergence, []) -> pure Nothing
            (Left info, _) -> do
                output (UnreachableCode info)
                pure Nothing
            (Right nextBlock, _) -> compileStatements nextBlock statements
    
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
        nextBlock <- reserveBlock 1
        let terminator = Call {
                    callFun = funName
                ,   callArgs = fmap (\(local, _) -> Copy (LocalPlace local)) (fromList exprsWithLocals)
                ,   destinationPlace = targetPlace
                ,   target = nextBlock
                }
        addBlock (finishBlock terminator block)
        -- Return empty block data for the new, currently unwritten 'nextBlock'
        pure emptyBlockData
    BinOp _ty left Add right -> do
        leftLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace leftLocal) currentBlock left
        rightLocal <- newAnonymousLocal Number
        block <- compileExprTo (LocalPlace rightLocal) block right
        pure $ addStatements [MIR.Assign targetPlace (PurePrimOp PrimAdd [Copy (LocalPlace (leftLocal)), Copy (LocalPlace (rightLocal))])] block
    C.Return () expr -> do
        lastBlock <- compileExprTo ReturnPlace currentBlock expr
        _ <- addBlock $ finishBlock MIR.Return lastBlock
        throw ReturnDivergence
    C.ExprBlock () statements retExpr -> do
        compileStatements currentBlock statements >>= \case
            -- The block diverges in all branches, so this expression necessarily diverges
            Nothing -> throw ReturnDivergence
            Just lastBlock -> compileExprTo targetPlace lastBlock retExpr
    C.If _ty condition thenBranch elseBranch -> do
        
        undefined

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

addBlock :: Members '[State FunState] r => BasicBlockData -> Sem r BasicBlock
addBlock newBlock = state (\s@FunState { blockData } -> 
    (BasicBlock { blockIndex = length blockData }, s { blockData = blockData |> newBlock }))

-- | Reserve a reference to the next 'nth' block (starting at 0!) without associating any data with it.
-- THIS REQUIRES THAT THE BLOCK WILL BE WRITTEN TO IMMEDIATELY!
-- Any subsequent call to `addBlock` (after `>= offset` calls to be exact) that is not
-- associated with this `reserveBlock` call, or any access of the block before an associated 
-- `addBlock` call is invalid and will almost definitely crash or produce wildly incorrect results!
reserveBlock :: Members '[State FunState] r => Int -> Sem r BasicBlock
reserveBlock offset = gets (\FunState {blockData} -> BasicBlock { blockIndex = length blockData + offset })

data FunState = FunState
    { nameLocals :: Map Name Int
    , nextLocal :: Int
    , localShapes :: Seq Shape
    , blockData :: Seq BasicBlockData
    }

shapeForType :: Type -> Shape
shapeForType IntT = Number
shapeForType BoolT = Number -- Booleans are desugared into numeric shapes. Maybe we should include some information in mir, so that we know checking for 0 and 1 is exhaustive?
shapeForType UnitT = Number -- TODO: This should be something zero sized, ideally something like `TupleShape []`
shapeForType NeverT = Number -- TODO: This should probably be zero sized? It's never going to be used anyway. Maybe we should just have a never shape?
