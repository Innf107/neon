module Neon.Rename (RenameError (..), rename, ModuleEnv, emptyModuleEnv) where

import Neon.Prelude
import Neon.Syntax

import Data.Map qualified as M

data RenameError
    = UnboundVar Text
    | UnboundFunction Text
    | DuplicateDefinition Text
    deriving (Show, Eq)

data ModuleEnv = ModuleEnv
    { functions :: Map Text Name
    , moduleName :: Text
    }
    deriving (Show)

emptyModuleEnv :: Text -> ModuleEnv
emptyModuleEnv moduleName =
    ModuleEnv
        { functions = mempty
        , moduleName
        }

data Env = Env
    { moduleEnv :: ModuleEnv
    , locals :: Map Text Name
    , currentFunctionName :: Name
    }
    deriving (Show)

insertFunction :: Text -> Name -> ModuleEnv -> ModuleEnv
insertFunction originalName name env@ModuleEnv{functions} = env{functions = insert originalName name functions}

insertLocal :: Text -> Name -> Env -> Env
insertLocal originalName name env@Env{locals} = env{locals = insert originalName name locals}

newFunction :: Members '[Error RenameError] r => ModuleEnv -> Text -> Sem r (Name, ModuleEnv)
newFunction env@ModuleEnv{functions, moduleName} originalName = case lookup originalName functions of
    Nothing ->
        let name = Name originalName moduleName 0
         in pure (name, insertFunction originalName name env)
    Just _ -> throw $ DuplicateDefinition originalName

freshLocal :: Env -> Text -> (Name, Env)
freshLocal env@Env{locals, currentFunctionName} originalName = do
    let name = case lookup originalName locals of
            Nothing -> Name originalName nameSource 0
            Just (Name _ _ i) -> Name originalName nameSource (i + 1)
    (name, insertLocal originalName name env)
  where
    -- This includes the module name since currentFunctionName is an already renamed Name
    nameSource = show currentFunctionName

rename :: Members '[Error RenameError] r => ModuleEnv -> [Decl Parsed] -> Sem r [Decl Renamed]
rename _ [] = pure []
rename modEnv (DefFunction () originalFunName params bodyStatements mReturnClause : decls) = do
    (funName, modEnv') <- newFunction modEnv originalFunName

    -- 'innerEnv' is derived from 'modEnv' *without* the definition for the current function since
    -- recursive calls (in tail position!) have their own special rules using 'currentFunctionName'
    let innerEnvBase = Env{moduleEnv = modEnv, locals = mempty, currentFunctionName = funName}
    let (params', innerEnv) =
            foldr
                ( \(paramName, ty) (rest, env) -> do
                    let (paramName', env') = freshLocal env paramName
                    ((paramName', ty) : rest, env')
                )
                ([], innerEnvBase)
                params

    (bodyStatements', returnClauseEnv) <- renameStatements innerEnv bodyStatements

    mReturnClause' <- case mReturnClause of
        Nothing -> pure Nothing
        Just (retExpr, retTy) -> do
            -- This doesn't have to do any special treatment around
            -- (tail) recursive calls, since these use the 'currentFunctionName'
            -- directly
            retExpr' <- renameExpr returnClauseEnv retExpr
            pure $ Just (retExpr', retTy)

    decls' <- rename modEnv' decls
    pure (DefFunction () funName params' bodyStatements' mReturnClause' : decls')

renameStatements :: Members '[Error RenameError] r => Env -> [Statement Parsed] -> Sem r ([Statement Renamed], Env)
renameStatements env [] = pure ([], env)
renameStatements env (DefVar () originalVarName value : statements) = do
    let (varName, env') = freshLocal env originalVarName

    -- This uses env, not env' since let bindings are (obviously) non recursive
    value' <- renameExpr env value
    
    (statements', env'') <- renameStatements env' statements
    pure (DefVar () varName value' : statements', env'')
renameStatements env (Perform () expr : statements) = do
    statement' <- Perform () <$> renameExpr env expr
    (statements', env) <- renameStatements env statements
    pure (statement' : statements', env)

renameExpr :: Members '[Error RenameError] r => Env -> Expr Parsed -> Sem r (Expr Renamed)
renameExpr _ (IntLit () i) = pure (IntLit () i)
renameExpr Env{locals} (Var () originalName) =
    case lookup originalName locals of
        Nothing -> throw $ UnboundVar originalName
        Just name -> pure (Var () name)
renameExpr env@Env{currentFunctionName, moduleEnv} (FCall () originalFunName args) = do
    funName <-
        -- TODO: Restrict this to only work in tail position
        if originalFunName == originalName currentFunctionName
            then pure currentFunctionName
            else case lookup originalFunName (functions moduleEnv) of
                Nothing -> throw $ UnboundFunction originalFunName
                Just funName -> pure funName

    args' <- traverse (renameExpr env) args
    pure (FCall () funName args')
renameExpr env (BinOp () left op right) = BinOp () <$> renameExpr env left <*> pure op <*> renameExpr env right
renameExpr env (Return () expr) = Return () <$> renameExpr env expr
renameExpr env (ExprBlock () statements retExpr) = do
    -- TODO: Use 'Seq' everywhere in here
    -- We only use 'innerEnv' in the retExpr, which maintains a separate scope (exactly what we want).
    -- The containing environment is *not* mutated by this.
    (statements', innerEnv) <- renameStatements env (toList statements)
    retExpr' <- renameExpr innerEnv retExpr
    pure (ExprBlock () (fromList statements') retExpr')
renameExpr env (If () condition thenBranch elseBranch) = do
    If () 
        <$> renameExpr env condition
        -- TODO: Variables with the same name in different branches might end up
        -- with the same renamed name. I'm not sure if this is an issue per se,
        -- but it is definitely something to be aware of.
        <*> renameExpr env thenBranch
        <*> renameExpr env elseBranch

