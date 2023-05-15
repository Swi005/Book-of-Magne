module Integers where

type Signature = ([Type],[FunDecl])
type Type = String
type FunDecl = (String, [Type], Type)
type FunModel valueDomain = String -> [valueDomain] -> valueDomain

-- | Signature for Integers
ints = (["Int"],[
            ("-", ["Int"], "Int"),
            ("+",["Int", "Int"],"Int"),
            ("*", ["Int", "Int"], "Int")
        ])


intSemantics :: FunModel Int
intSemantics "+" = \[x,y] -> x + y
intSemantics "*" = \[x,y] -> x * y
intSemantics "-" = \[x] -> -x
intSemantics "0" = \[] -> 0
intSemantics "1" = \[] -> 1

