module Calcite.Typecheck where

import Calcite.Prelude
import Calcite.Types.AST

-- Whenever two types are given, the first one was 'expected', while the second was 'provided'.
data TypeError = WrongFunctionReturn Name (Type Typed) (Type Typed) 
               | NonFunctionCall Name (Type Typed)
               | WrongNumberOfParams Name [Type Typed] [Type Typed]
               | MismatchedParameter Name (Type Typed) (Type Typed)
               deriving (Show, Eq)

data TCState = TCState {
    varTypes :: Map Name (Type Typed)
}

insertType :: Members '[State TCState] r => Name -> Type Typed -> Sem r ()
insertType x t = modify (\s -> s{varTypes = insert x t (varTypes s)})

lookupType :: Members '[State TCState] r => Name -> Sem r (Type Typed)
lookupType n = (lookup n <$> gets varTypes) <&> \case
    Nothing -> error $ "lookupType: No type for variable: " <> show n
    Just ty -> ty

typecheck :: Members '[State TCState, Error TypeError] r => [Decl Renamed] -> Sem r [Decl Typed]
typecheck = traverse tcDecl

tcDecl :: Members '[State TCState, Error TypeError] r => Decl Renamed -> Sem r (Decl Typed)
tcDecl (DefFunction NoExt f (map (second cast) -> xs) (cast -> retTy) sts retExp) = do
    insertType f (FunT (map snd xs) retTy)
    traverse_ (uncurry insertType) xs
    
    retExp' <- tcExpr retExp
    let retExpTy = getType retExp'
    when (not (retExpTy `subTypeOf` retTy)) $ throw $ WrongFunctionReturn f retTy retExpTy

    DefFunction NoExt f xs retTy <$> traverse tcStmnt sts <*> pure retExp'
tcDecl (DefProc NoExt f (map (second cast) -> xs) sts) = do
    insertType f (ProcT (map snd xs))
    traverse_ (uncurry insertType) xs

    DefProc NoExt f xs <$> traverse tcStmnt sts


tcStmnt :: Members '[State TCState, Error TypeError] r => Statement Renamed -> Sem r (Statement Typed)
tcStmnt (DefVar NoExt x e) = DefVar NoExt x <$> tcExpr e

subTypeOf :: Type Typed -> Type Typed -> Bool
subTypeOf = (==) -- fine for now, as there are no actual subtypes yet.

tcExpr :: Members '[State TCState, Error TypeError] r => Expr Renamed -> Sem r (Expr Typed)
tcExpr (IntLit NoExt n) = pure $ IntLit NoExt n
tcExpr (Var NoExt x) = do
    ty <- lookupType x
    pure (Var ty x)
tcExpr (FCall NoExt f args) = do
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

