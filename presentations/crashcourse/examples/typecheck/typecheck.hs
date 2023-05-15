module TypeCheckExample where
type VarDecl = [(String, ExprType)]

data Expr = 
        Var String
        | I Int
        | B Bool
        | BinOp Op Expr Expr
        | UnOp Op Expr
        | Choice Expr Expr Expr deriving (Show, Eq)
data Op = Plus | Mult | Or | And | Not | Eq deriving (Show, Eq)
data ExprType = Integer | Boolean deriving (Show, Eq)

typeCheck ::VarDecl->Expr->ExprType
typeCheck _ (I _) = Integer
typeCheck _ (B _) = Boolean

typeCheck vars (Var name) = 
                case lookup name vars of
                        Just tp -> tp
                        Nothing -> error $ "No variable could be found named "++name

typeCheck vars (UnOp Not expr) = 
                        case typeCheck vars expr of
                                Boolean-> Boolean 
                                _-> error "Argument not boolean"

typeCheck vars (BinOp Plus left right) = 
                        case (typeCheck vars left, typeCheck vars right) of
                                (Integer, Integer)->Integer
                                _->error "One of the args is not an Integer"

typeCheck vars (BinOp Mult left right) = 
                        case (typeCheck vars left, typeCheck vars right) of
                                (Integer, Integer)->Integer
                                _->error "One of the args is not an Integer"

typeCheck vars (BinOp Or left right) = 
                        case (typeCheck vars left, typeCheck vars right) of
                                (Boolean, Boolean)->Boolean
                                _->error "One of the args is not a Boolean"

typeCheck vars (BinOp And left right) = 
                        case (typeCheck vars left, typeCheck vars right) of
                                (Boolean, Boolean)->Boolean
                                _->error "One of the args is not a Boolean"

typeCheck vars (BinOp Eq left right) = Boolean

typeCheck vars (Choice test left right) = 
                        case typeCheck vars test of
                                Boolean | l == r -> r
                                        | otherwise -> error "Args did not match"
                                _-> error "Test condition is not a Boolean"
                        where
                                l = typeCheck vars left
                                r = typeCheck vars right