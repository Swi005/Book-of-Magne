module Intrinsics where


type Signature  = ([Sort], [FunDecl])

type Sort = String
type FunDecl = (String, [Sort], Sort)


intrinsics :: Signature
intrinsics = (["Int", "Bool"],[
                ("add", ["Int", "Int"], "Int"),
                ("sub", ["Int", "Int"], "Int"),
                ("mult", ["Int", "Int"], "Int"),
                ("div", ["Int", "Int"], "Int"),
                ("leq", ["Int", "Int"], "Bool")
            ])

-- | 
data VD = Bool Bool | Int Int

intrinsicSemantics :: String -> [VD] -> VD
intrinsicSemantics "add" [Int a, Int b] = Int (a + b)
intrinsicSemantics "sub" [Int a, Int b] = Int (a - b)
intrinsicSemantics "mult" [Int a, Int b] = Int (a * b)
intrinsicSemantics "div" [Int a, Int 0] = error $ "Division by zero: " ++ show a ++ " / 0"
intrinsicSemantics "div" [Int a, Int b] = Int (div a b)
intrinsicSemantics "leq" [Int a, Int b] = Bool (a <= b)