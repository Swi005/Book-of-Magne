module AST where


type Env = [(String, Int)]

data Expr 
    = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | LEQ Expr Expr
    | Var String
    deriving (Show, Eq)

eval :: Env -> Expr -> Int
eval _ (Lit n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mult e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = if eval env e2 /= 0 then div (eval env e1) (eval env e2) else error $ "Division by zero: " ++ show e1 ++ " / " ++ show e2
eval env (LEQ e1 e2) = if eval env e1 <= eval env e2 then 1 else 0
eval env (Var string) = case lookup string env of
                        Just n -> n
                        Nothing -> error $ "Variable " ++ string ++ " not found in environment"