module Calcite.Codegen where

import Calcite.Prelude hiding (Local)
import Calcite.Types.AST

import Language.McFunction.Types

compile :: [Decl Typed] -> Sem r [CompiledModule]
compile = traverse compileDecl

data FunctionState = FunctionState {
        varLocations :: Map Name VarLocation
    ,   functionName :: Name
    } deriving (Show, Eq)

emptyFunctionState :: Name -> FunctionState
emptyFunctionState = FunctionState mempty

data VarLocation = Local
                 | Arg Int 
                 deriving (Show, Eq)

calciteObj :: Objective
calciteObj = "calcite"

compileDecl :: Decl Typed -> Sem r CompiledModule
compileDecl (DefFunction NoExt f xs _retTy sts retE) = compileFunctionOrProc f xs sts (Just retE) 
compileDecl (DefProc NoExt f xs sts) = compileFunctionOrProc f xs sts Nothing

compileFunctionOrProc :: Name
    -> [(Name, Type Typed)]
    -> [Statement Typed]
    -> (Maybe (Expr Typed))
    -> Sem r (String, [Command])
compileFunctionOrProc f xs sts mRetE = evalState (emptyFunctionState f) do
    argcmds <- concat <$> zipWithM (\(x, t) i -> compileArg x t i ) xs [0..]
    stmntcmds <- concat <$> traverse compileStatement sts

    returnCommands <- case mRetE of
        Nothing   -> pure [] 
        Just retE -> case getType retE of
            IntT -> compileExprToScore (NamespacedPlayer (Own (renderName (retName f)))) calciteObj retE
            ty -> error $ "compileFunctionOrProc: invalid function return type: " <> show ty

    pure  (toString (renderName f), argcmds <> stmntcmds <> returnCommands)
    where
        compileArg :: Members '[State FunctionState] r => Name -> Type Typed -> Int -> Sem r [Command]
        compileArg x IntT i = do
            modify (\s -> s{varLocations = insert x (Arg i) (varLocations s)})
            pure []
        compileArg x t i = error $ "compileDecl: cannot compile arguments of type '" 
                                <> show t <> "' for argument '" <> renderName x <> "' of function '"
                                <> renderName f <> "' at index " <> show i

compileStatement :: Members '[State FunctionState] r => Statement Typed -> Sem r [Command]
compileStatement (DefVar NoExt x e) = do
    modify (\s -> s{varLocations = insert x Local (varLocations s)}) 
    compileExprToScore (NamespacedPlayer (Own $ renderName x)) calciteObj e


argName :: Name -> Int -> Name
argName (Name fname j) i = Name (fname <> "-arg-" <> show i) j

retName :: Name -> Name
retName (Name fname j) = Name (fname <> "-ret") j

compileExprToScore :: Members '[State FunctionState] r => Selector -> Objective -> Expr Typed -> Sem r [Command]
compileExprToScore s o (IntLit NoExt n) = pure [Scoreboard (Players (Set s o n))]
compileExprToScore s o (Var IntT x) = gets functionName >>= \fname -> gets (lookup x . varLocations) <&> \case
    Just (Arg i)    -> [Scoreboard (Players (Operation s o SAssign (NamespacedPlayer (Own $ renderName $ argName fname i)) calciteObj))]
    Just Local      -> [Scoreboard (Players (Operation s o SAssign (NamespacedPlayer (Own $ renderName x)) calciteObj))]
    Nothing -> error $ "compileExprToScore: cannot find location of variable '" <> renderName x <> "' when assigning to '" <> show s <> "' '" <> show o <> "'"
compileExprToScore s o (Var t x) = error $ "compileExprToScore: invalid variable type: '" <> show t <> "' when assigning variable '" <> renderName x <> "' to '" <> show s <> "' '" <> show o <> "'"
compileExprToScore s o (FCall retTy f args) = do
    argCommands <- concat <$> zipWithM (\i arg -> compileExprToScore (NamespacedPlayer (Own $ renderName $ argName f i)) calciteObj arg) [0..] args 
    pure ( argCommands
        <> [Function (Own (renderName f))]
        <> retCommands)
        where retCommands = case retTy of
                IntT -> [Scoreboard (Players (Operation s o SAssign (NamespacedPlayer (Own $ renderName (retName f))) calciteObj))] -- TODO
                _    -> error $ "compileExprToScore: invalid function return type: " <> show retTy

