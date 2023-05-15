module Booleans where

type Signature = ([Type],[FunDecl])
type Type = String
type FunDecl = (String, [Type], Type)
type FunModel valueDomain = String -> [valueDomain] -> valueDomain

-- | Signature for Integers
bool = (["Bool"],[
            ("and",["Bool", "Bool"],"Bool"),
            ("not", ["Bool"], "Bool"),
            ("or", ["Bool", "Bool"], "Bool")
        ])


-- | Algebra for Booleans using Haskell Bool as domain
boolSemantics :: FunModel Bool
boolSemantics "and" = \[x,y] -> x && y
boolSemantics "not" = \[x] -> not x
boolSemantics "or" = \[x,y] -> x || y
boolSemantics "False" = \[] -> False
boolSemantics "True" = \[] -> True

-- | Algebra for Booleans using Haskell Int as domain
boolSemantics' :: FunModel Int
boolSemantics' "and" = \[x,y] -> if x+y == 2 then 1 else 0
boolSemantics' "not" = \[x] -> if x == 0 then 1 else 0
boolSemantics' "or" = \[x,y] -> if x+y == 0 then 0 else 1
boolSemantics' "False" = \[] -> 0
boolSemantics' "True" = \[] -> 1