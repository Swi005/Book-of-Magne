module TypeInferExample where

type SimpleEnviroment = [(String, Expr)]

data Expr = 
        Var String
        | IntLit Integer
        | StrLit String
        | Plus Expr Expr
        | Mult Expr Expr

--New Rules
--Int + string = string
--String+String = string
--String * int = String

data ExprType = Integer | String deriving (Show, Eq)

inferType:: SimpleEnviroment->Expr->ExprType
inferType _ (IntLit _) = Integer
inferType _ (StrLit _) = String

inferType env (Var name) = case lookup name env of
                        Just expr -> inferType env expr
                        _ -> error "Can't find var"

inferType env (Plus left right) = case (inferType env left, inferType env right) of
                                (Integer, Integer)->Integer
                                (_,_)->String --If it's not two ints it's a string!

ingerType env (Mult left right) = case (inferType env left, inferType env right) of
                                (String, Integer)-> String
                                (Integer, Integer)->Integer
                                _->error "invalid arguments"