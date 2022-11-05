module Calcite.Typecheck where

import Calcite.Prelude
import Calcite.Types.AST

-- Whenever two types are given, the first one was 'expected', while the second was 'provided'.
data TypeError = WrongFunctionReturn Name Type Type 
               | NonFunctionCall Name Type
               | WrongNumberOfParams Name [Type] [Type]
               | MismatchedParameter Name Type Type
               deriving (Show, Eq)

data TCState = TCState {
    varTypes :: Map Name Type
}

insertType :: Members '[State TCState] r => Name -> Type -> Sem r ()
insertType x t = modify (\s -> s{varTypes = insert x t (varTypes s)})

lookupType :: Members '[State TCState] r => Name -> Sem r Type
lookupType n = (lookup n <$> gets varTypes) <&> \case
    Nothing -> error $ "lookupType: No type for variable: " <> show n
    Just ty -> ty

typecheck :: Members '[State TCState, Error TypeError] r => [Decl Renamed] -> Sem r [Decl Typed]
typecheck = traverse tcDecl

tcDecl :: Members '[State TCState, Error TypeError] r => Decl Renamed -> Sem r (Decl Typed)
tcDecl (DefFunction () f xs sts (Just (retExp, retTy))) = do
    insertType f (FunT (map snd xs) retTy)
    traverse_ (uncurry insertType) xs
    
    retExp' <- tcExpr retExp
    let retExpTy = getType retExp'
    when (not (retExpTy `subTypeOf` retTy)) $ throw $ WrongFunctionReturn f retTy retExpTy

    sts' <- traverse tcStmnt sts

    pure $ DefFunction () f xs sts' (Just (retExp', retTy))
tcDecl (DefFunction () f xs sts Nothing) = do
    insertType f (ProcT (map snd xs))
    traverse_ (uncurry insertType) xs

    DefFunction () f xs <$> traverse tcStmnt sts <*> pure Nothing


tcStmnt :: Members '[State TCState, Error TypeError] r => Statement Renamed -> Sem r (Statement Typed)
tcStmnt (DefVar () x e) = DefVar () x <$> tcExpr e
tcStmnt (Perform () e)  = Perform () <$> tcExpr e

subTypeOf :: Type -> Type -> Bool
subTypeOf = (==) -- fine for now, as there are no actual subtypes yet.

tcExpr :: Members '[State TCState, Error TypeError] r => Expr Renamed -> Sem r (Expr Typed)
tcExpr (IntLit () n) = pure $ IntLit () n
tcExpr (Var () x) = do
    ty <- lookupType x
    pure (Var ty x)
tcExpr (FCall () f args) = do
    fty <- lookupType f
    case fty of
        FunT tys retTy -> do
            args' <- traverse tcExpr args
            let argTys = map getType args'

            when (length args /= length argTys) $ throw $ WrongNumberOfParams f tys argTys

            zipWithM_ checkArgType tys argTys

            pure (FCall retTy f args')
        _ -> throw $ NonFunctionCall f fty
        where
            checkArgType ty argTy = when (not (argTy `subTypeOf` ty)) $ throw $ MismatchedParameter f ty argTy
tcExpr (Return () expr) =
    undefined
