module Calcite.Rename where

import Calcite.Prelude
import Calcite.Types.AST

import Data.Map qualified as M

data RenameError = UndefinedVar Text deriving (Show, Eq)

data RenamerState = RenamerState {
        renamerVars :: Map Text Int
    } deriving (Show, Eq)

insertVar :: Text -> RenamerState -> (RenamerState, Name)
insertVar n s = let (n', m') = M.alterF updateMap n (renamerVars s) in (s{renamerVars=m'}, n')
    where
        updateMap = \case
            Nothing -> (Name n 0, Just 0)
            Just i  -> (Name n (i + 1), Just (i + 1))


lookupVar :: Members '[Error RenameError] r => Text -> RenamerState -> Sem r Name 
lookupVar n RenamerState { renamerVars } = case lookup n renamerVars of
    Nothing -> throw $ UndefinedVar n
    Just i  -> pure $ Name n i


(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

rename :: Members '[Error RenameError] r => RenamerState -> [Decl Parsed] -> Sem r [Decl Renamed]
rename _ [] = pure []
rename s (DefFunction () f xs sts (Just (retExp, retTy)) : ds) = do
    let (s', f') = insertVar f s
    -- TODO: Rename parameter x to "<f>_<x>" and detect duplicates
    let (sInner, xs') = foldr (\(n, t) (r, xs) -> let (r', n') = insertVar n r in (r', (n', t) : xs)) (s, []) xs
    --                        s *not* s', because recursive functions should be implemented with 'rec' ^ 
    sts' <- renameStmnts sInner sts
    retExp' <- renameExpr sInner retExp
    (DefFunction () f' xs' sts' (Just (retExp', retTy)) :) <$> rename s' ds
rename s (DefFunction () f xs sts Nothing : ds) = undefined 
    
renameStmnts :: Members '[Error RenameError] r => RenamerState -> [Statement Parsed] -> Sem r [Statement Renamed]
renameStmnts _ [] = pure []
-- TODO: Variable x should be renamed to "<f>_<x>" where f is the name of the enclosing function
renameStmnts s (DefVar () x e : ds) = do
    let (s', x') = insertVar x s
    (DefVar () x' <$> renameExpr s e)
        <:> renameStmnts s' ds

renameExpr :: Members '[Error RenameError] r => RenamerState -> (Expr Parsed) -> Sem r (Expr Renamed)
renameExpr _ (IntLit () n)   = pure $ IntLit () n
-- TODO: Variable x should be searched for as "<f>_<x>" where f is the name of the enclosing function
renameExpr s (Var () x)      = Var () <$> lookupVar x s
renameExpr s (FCall () f xs) = FCall () <$> lookupVar f s <*> traverse (renameExpr s) xs



