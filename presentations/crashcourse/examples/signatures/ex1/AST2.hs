module AST2 where


type Env valueDomain = [(String, valueDomain)]
data Expr valueDomain 
            = Lit valueDomain
            | Var String
            | FunCall String [Expr valueDomain]
            

eval :: (String -> [valueDomain] -> valueDomain) -> Env valueDomain ->Expr valueDomain -> valueDomain
eval fmod env (Lit n) = n
eval fmod env (Var string) = case lookup string env of
                        Just n -> n
                        Nothing -> error $ "Variable " ++ string ++ " not found in environment"
eval fmod env (FunCall f args) = fmod f (map (eval fmod env) args)