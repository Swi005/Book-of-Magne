module Naturals where

type Signature = ([Type],[FunDecl])
type Type = String
type FunDecl = (String, [Type], Type)
type FunModel valueDomain = String -> [valueDomain] -> valueDomain

-- | Signature for Nategers
nats :: Signature
nats = (["Natural"],[
            ("+",["Nat", "Nat"],"at"),
            ("*", ["Nat", "Nat"], "Nat")
        ])


-- We also use Int as our carrier set, which is not ideal, but with proper checks can emulate Nat
natSemantics :: FunModel Int
natSemantics "+" = \[x,y] -> x + y
natSemantics "*" = \[x,y] -> x * y
natSemantics "0" = \[] -> 0
natSemantics "1" = \[] -> 1
