module AST where

type Signature = ([Type],[FunDecl])
type Type = String
type FunDecl = (String, [Type], Type)
type FunModel valueDomain = String -> [valueDomain] -> valueDomain

type SimpleEnviroment valuedomain = [(String, valuedomain)]

-- | Very simple language, 
data Expr valueDomain = Lit valueDomain
            | Var String
            | FunCall String [Expr valueDomain]
            deriving (Show, Eq)

data Stmt valueDomain = AssertEq (Expr valueDomain) (Expr valueDomain)
                        | DeclVar String (Expr valueDomain)-- DeclVar and SetVar could probably be combined
                        | SetVar String (Expr valueDomain)
                      
                deriving (Show, Eq)


-- | Eval evaluates an expression resulting in some value
eval :: Eq valueDomain => FunModel valueDomain ->SimpleEnviroment valueDomain -> Expr valueDomain -> valueDomain
eval _ env (Lit x) = x
eval _ env (Var x) = case lookup x env of
                        Just v -> v
                        Nothing -> error ("Variable " ++ x ++ " not found")
eval fmod env (FunCall f args) = fmod f (map (eval fmod env) args)



-- | Exec executes a statement resulting a change in enviroment
exec :: Eq valueDomain => FunModel valueDomain -> SimpleEnviroment valueDomain -> Stmt valueDomain -> SimpleEnviroment valueDomain
exec fmod env (DeclVar x e) = case lookup x env of
                                Just v -> error $ x ++" is allready declared" --bad practice to do checks at runtime, should be part of static analysis portion of program
                                Nothing -> (x, eval fmod env e):env
exec fmod env (SetVar x e) = case lookup x env of
                                Just v -> (x, eval fmod env e):env
                                Nothing -> error $ x ++ " is not declared"

exec fmod env (AssertEq e1 e2) = if eval fmod env e1 == eval fmod env e2 
                                    then env
                                else error "Assertion failed"
exec fmod env (Assert e1) = if eval fmod env e1 then env else error "Assertion failed" 

-- | Checks that a signature is well-formed
checkSignature :: Signature -> Bool
checkSignature (tps, funcs) = all (checkFunDecl tps) funcs
    where
        checkFunDecl tps (f, args, res) = all (`elem` tps) (res:args)