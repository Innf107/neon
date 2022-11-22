module Neon.Typecheck where

import Neon.Prelude
import Neon.Syntax

-- Whenever two types are given, the first one was 'expected', while the second was 'provided'.
data TypeError = NotASubType Type Type
               | WrongNumberOfArgs Name Int Int
               deriving Show

data FunType = MkFunType (Seq Type) Type

data TCDeclEnv = TCDeclEnv {
    funTypes :: Map Name FunType
}

emptyTCDeclEnv :: TCDeclEnv
emptyTCDeclEnv = TCDeclEnv {
    funTypes = mempty
}

data TCEnv = TCEnv {
    localTypes :: Map Name Type
}

emptyTCEnv :: TCEnv
emptyTCEnv = TCEnv {
    localTypes = mempty
}

data EnclosingFunEnv = EnclosingFunEnv {
    returnType :: Type
}

typecheckDecls :: Members '[Error TypeError] r => Seq (Decl Renamed) -> Sem r (Seq (Decl Typed))
typecheckDecls decls = evalState emptyTCDeclEnv $ traverse typecheckDecl decls

insertFunType :: Members '[State TCDeclEnv] r => Name -> FunType -> Sem r ()
insertFunType funName funType =
    modify (\s@TCDeclEnv{funTypes} -> s{funTypes = insert funName funType funTypes})

insertLocalType :: Name -> Type -> TCEnv -> TCEnv
insertLocalType name ty env@TCEnv{localTypes} = env { localTypes = insert name ty localTypes }
    

typecheckDecl :: Members '[Error TypeError, State TCDeclEnv] r
              => Decl Renamed
              -> Sem r (Decl Typed)
typecheckDecl = \case 
    DefFunction () funName parameters retTy body retExpr -> do
            
        let funType = MkFunType (fromList (map snd parameters)) retTy
        
        -- We insert the type immediately to allow recursion.
        insertFunType funName funType

        let env = foldr (\(name, ty) env -> insertLocalType name ty env) emptyTCEnv parameters

        runReader (EnclosingFunEnv { returnType = retTy }) do
            (env, body') <- runState env $ traverse typecheckStatement body
        
            retExpr' <- check env retTy retExpr
            pure (DefFunction () funName parameters retTy body' retExpr')


typecheckStatement :: Members '[Error TypeError, State TCDeclEnv, State TCEnv, Reader EnclosingFunEnv] r
                   => Statement Renamed
                   -> Sem r (Statement Typed)
typecheckStatement = \case
    DefVar () varName expr -> do
        env <- get
        expr' <- infer env expr
        modify (insertLocalType varName (getType expr'))
        pure $ DefVar () varName expr'
    Perform () expr -> do
        env <- get
        Perform () <$> check env UnitT expr
    InlineAsm () components -> undefined

check :: Members '[Error TypeError, State TCDeclEnv, Reader EnclosingFunEnv] r
      => TCEnv
      -> Type
      -> Expr Renamed
      -> Sem r (Expr Typed)
check env ty = \case
    ExprBlock () statements expr -> do
        (env, statements') <- runState env $ traverse typecheckStatement statements
        ExprBlock () statements' <$> check env ty expr
    If () condition thenBranch elseBranch ->
        If ty
        <$> check env BoolT condition
        <*> check env ty thenBranch
        <*> check env ty elseBranch
    expr -> do
        expr' <- infer env expr
        subsumes (getType expr') ty
        pure expr'

infer :: Members '[Error TypeError, State TCDeclEnv, Reader EnclosingFunEnv] r
      => TCEnv
      -> Expr Renamed
      -> Sem r (Expr Typed)
infer env = \case
    IntLit () n -> pure (IntLit () n)
    UnitLit () -> pure (UnitLit ())
    Var () varName -> do
        let ty = case lookup varName (localTypes env) of
                Nothing -> error $ "Variable not found during type checking: '" <> show varName <> "'"
                Just ty -> ty
        pure (Var ty varName)
    FCall () funName arguments -> do
        -- We don't need to deal with polymorphism yet, so this is pretty simple.
        MkFunType argTys resTy <- gets (lookup funName . funTypes) <&> \case
                Nothing -> error $ "Function not found during type checking: '" <> show funName <> "'"
                Just ty -> ty
        when (length arguments /= length argTys) $ throw (WrongNumberOfArgs funName (length argTys) (length arguments))

        arguments' <- zipWithM (\arg argTy -> check env argTy arg) arguments (toList argTys)
        
        pure (FCall resTy funName arguments')
    BinOp () left Add right -> do
        left' <- check env IntT left
        right' <- check env IntT right
        pure (BinOp IntT left' Add right')
    BinOp () left LE right -> do
        left' <- check env IntT left
        right' <- check env IntT right
        pure (BinOp BoolT left' LE right')
    Return () retExpr -> do
        EnclosingFunEnv { returnType } <- ask
        retExpr' <- check env returnType retExpr
        pure (Return () retExpr')
    ExprBlock () statements retExpr -> do
        (env, statements') <- runState env $ traverse typecheckStatement statements
        retExpr' <- infer env retExpr
        pure (ExprBlock () statements' retExpr')
    If () condition thenBranch elseBranch -> do
        condition' <- check env BoolT condition
        thenBranch' <- infer env thenBranch
        elseBranch' <- infer env elseBranch
        
        -- We need to make sure that both branches have compatible types, so
        -- we check that they are mutual subtypes. This is significantly more elegant
        -- in check mode.
        subsumes (getType thenBranch') (getType elseBranch')
        subsumes (getType elseBranch') (getType thenBranch')
        pure (If (getType thenBranch') condition' thenBranch' elseBranch')


-- Makes sure that ty1 is a subtype of ty2
subsumes :: Members '[Error TypeError] r => Type -> Type -> Sem r ()
subsumes NeverT _ = pure ()
subsumes _ NeverT = pure ()
subsumes IntT IntT = pure ()
subsumes BoolT BoolT = pure ()
subsumes UnitT UnitT = pure ()
subsumes ty1 ty2 = throw (NotASubType ty1 ty2)
