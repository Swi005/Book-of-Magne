data Expr = Plus Expr Expr
            | Mult Expr Expr
            | Or Expr Expr

data Expr = BinOp 