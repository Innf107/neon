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
compileDecl (C.DefFunction () funName params statements mreturn) = do
    let paramShapes = map (shapeForType . snd) params
    let funState =
            FunState
                { nameLocals = fromList $ zipWith (\(x, _) i -> (x, i)) params [0 ..]
                , nextLocal = length params
                , localShapes = fromList paramShapes
                , blockData = []
                }

    (statements, returnShape) <- pure $ case mreturn of
            Nothing -> (fromList statements, Number) -- TODO: Shape for Unit
            Just (retExp, ty) -> (fromList statements |> Perform () (C.Return () retExp), shapeForType ty)

    (state, _) <- runState funState $ compileStatements emptyBlockData statements

    let FunState { localShapes, blockData } = state

    pure $ MIR.DefFunction funName (length params) (localShapes) returnShape (Body { blocks=blockData })

compileStatements :: Members '[Output LowerWarning, State FunState] r => PartialBlockData -> Seq (C.Statement Typed) -> Sem r ()
compileStatements currentBlock = \case 
    Empty -> do
        _ <- addBlock 
            $ finishBlock MIR.Return 
            $ addStatements [Assign ReturnPlace (Use (Literal UnitLit))] currentBlock
        pure ()
    DefVar () varName expr :<| statements -> do
        local <- newLocal varName (shapeForType (getType expr))
        runError (compileExprTo (LocalPlace local) currentBlock expr) >>= \case
            Left info -> do
                output (UnreachableCode info)
            Right nextBlock -> 
                compileStatements nextBlock statements
    Perform () expr :<| statements -> do
        mnextBlock <- runError $ compileExprTo WildCardPlace currentBlock expr
        case (mnextBlock, statements) of
            (Left ReturnDivergence, []) -> pure ()
            (Left info, _) -> do
                output (UnreachableCode info)
            (Right nextBlock, _) -> compileStatements nextBlock statements
    
compileExprTo :: Members '[State FunState, Error DivergenceInfo] r => Place -> PartialBlockData -> C.Expr Typed -> Sem r PartialBlockData
compileExprTo targetPlace currentBlock = \case
    C.IntLit () n -> do
        let block = addStatements [MIR.Assign targetPlace (Use (Literal (MIR.IntLit n)))] currentBlock
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
shapeForType IntT = Number -- We only support numbers for now
shapeForType (FunT _ _) = Number -- TODO: I honestly don't think these should be *types*, since locals can never have these
shapeForType (ProcT _) = Number --  ^
shapeForType NeverT = Number -- TODO: This should probably be zero sized? It's never going to be used anyway. Maybe we should just have a never shape?
