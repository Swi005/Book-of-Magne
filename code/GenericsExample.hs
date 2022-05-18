module IntrinsicExample where

type TypeName = String
type FunName = String
type VarName = String

type Enviroment valuedomain= [(String, ExprAST valuedomain)]
--Defines semantics of a signature
type FunModel valuedomain = FunName -> [valuedomain] -> valuedomain

type FunDeclaration = (FunName, [TypeName], TypeName)
type TypeDeclaration = TypeName

--Defines what functions types exists
type Signature = ([TypeDeclaration],[FunDeclaration])

--DS??
data ExprAST valuedomain
    = Lit valuedomain TypeName
    | Fun FunName [ExprAST valuedomain]
    | Var VarName
    | Assert (ExprAST valuedomain)
    deriving (Eq, Read, Show)


--DSL Describing Ints
int ::Signature
int = (["Int"],[
                ("Add", ["Int","Int"],"Int"),
                ("Mult", ["Int","Int"], "Int"),
                ("Neg", ["Int"], "Int"),
                ("Eq", ["Int", "Int"], "Int"),
                ("LEq", ["Int", "Int"], "Int")
        ])

--DSL for Describing Reals
real :: Signature
real =(["Real"],[
                ("Add", ["Real","Real"],"Real"),
                ("Mult", ["Real","Real"], "Real"),
                ("Neg", ["Real"],"Real"),
                ("Eq", ["Real", "Real"], "Real"),
                ("LEq", ["Real", "Real"], "Real")
    ])
--Natural nums
nat :: Signature 
nat =(["Nat"],[
                ("Add", ["Nat","Nat"],"Nat"),
                ("Mult", ["Nat","Nat"], "Nat"),
                ("Eq", ["Nat", "Nat"], "Nat"),
                ("LEq", ["Nat", "Nat"], "Nat")
    ])

--Semantics of a ring
ringModel ::Num valuedomain=>Show valuedomain=>Ord valuedomain=>Eq valuedomain=>FunModel valuedomain
ringModel "Add" arg =  foldl (+) 0 arg
ringModel "Mult" arg = foldl (*) 1 arg
ringModel "Neg" [x] = negate x
ringModel "Eq" [x, y] | x==y = 1 |otherwise =  0--Emulate booleans wiht int 0=false, 1=true
ringModel "LEq" [x, y] | x<=y = 1 | otherwise  = 0

eval ::Num valuedomain=>Show valuedomain=>Ord valuedomain=>Eq valuedomain=>FunModel valuedomain->Signature->Enviroment valuedomain->ExprAST valuedomain->valuedomain
eval mod sig env (Lit i _) = i
eval mod sig env (Var name) = case lookup name env of
                            Just x -> eval mod sig env x
                            Nothing -> error "No such var in env"

eval mod (types, funcs) env (Fun name args) | elem name (map (\(x,y,z)->x) funcs) =mod name (map (eval mod (types, funcs) env) args)
                                            | otherwise= error "Can't find function in signature"

eval mod (sig) env (Assert expr) = let res = eval mod sig env expr in if res == 1 then res else error $"Assert did not hold"++ (show expr)


--Tests that carrier set is abelian group
testAssoc :: ExprAST valuedomain--We use Integer to represent the set 
testAssoc = 
    Assert (
        Fun "Eq" [
            Fun "Add" [
                Fun "Add" [
                    Var"a",
                    Var"b"
                    ], 
                Var"c"
            ],
            Fun "Add" [
                Var"a", 
                Fun "Add" 
                [
                    Var"b", 
                    Var"c"
                ]
            ]
        ]
    )

testAddComm ::ExprAST valuedomain
testAddComm = 
    Assert (
        Fun "Eq" [
            Fun "Add" [
                Var "a",
                Var "b"
            ],
            Fun "Add" [
                Var "b",
                Var"a"
            ]
        ]
    )

testAddID ::ExprAST valuedomain
testAddID = 
    Assert (
        Fun "Eq" [
            Fun "Add" [
                Var "a",
                Var "addConst"],
            Var"a"
        ]
    )

testAddInv ::ExprAST valuedomain
testAddInv = 
    Assert (
        Fun "Eq" [
            Fun "Add" [
                Var"a",
                Fun "Neg" [
                    Var"a"
                ]
            ],
            Var "addConst"
        ]
    )

--Tests that carrier set is a monoid under multiplication
testMultAssoc::ExprAST valuedomain
testMultAssoc = 
    Assert (
        Fun "Eq" [
            Fun "Mult" [
                Fun "Mult" [
                    Var"a",
                    Var"b"
                ],
                Var "c"
            ],
            Fun "Mult" [
                Var"a", 
                Fun "Mult" [
                    Var"b",
                    Var"c"
                ]
            ]
        ]
    )

testMultID ::ExprAST valuedomain
testMultID = 
    Assert (
        Fun "Eq" [
            Fun "Mult" [
                    Var"a", 
                    Var "multConst"
                ],
            Var "a"
        ]
    )

--Test that multiplication is distributive
testLDist :: ExprAST valuedomain
testLDist = 
    Assert (
        Fun "Eq" [
            Fun "Mult" [
                Var "a", 
                Fun "Add" [Var "b",Var "c"]
            ], 
            Fun "Add" [
                (Fun "Mult" [Var"a",Var "b"]),
                (Fun "Mult" [Var"a",Var "c"])
            ] 
        ]
    )

testRDist :: ExprAST valuedomain
testRDist =  
    Assert ( 
        Fun "Eq" [
            Fun "Mult" [
                Fun "Add" [Var "b", Var"c"], 
                Var"a"
                ], 
            Fun "Add" [
                (Fun "Mult" [Var "b",Var "a"]),
                (Fun "Mult" [Var "c",Var "a"])
                ] 
        ]
    )



tests = [testAssoc, testAddComm, testAddID, testAddInv, testMultAssoc, testMultID, testLDist, testRDist]

test tpname (sig, model) dta= let env = [("a", Lit (dta!!0) tpname), ("b", Lit (dta!!1) tpname), ("c", Lit (dta!!2) tpname), ("multConst", Lit 1 tpname), ("addConst", Lit 0 tpname)] in
    map (eval model sig env) tests