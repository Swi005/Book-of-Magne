module GenericsExample where

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
int = (["Int", "Bool"],[
                ("Add", ["Int","Int"],"Int"),
                ("Mult", ["Int","Int"], "Int"),
                ("Neg", ["Int"], "Int"),
                ("Eq", ["Int", "Int"], "Bool"),
                ("LEq", ["Int", "Int"], "Bool")
        ])

--DSL for Describing Reals
real :: Signature
real =(["Real", "Bool"],[
                ("Add", ["Real","Real"],"Real"),
                ("Mult", ["Real","Real"], "Real"),
                ("Neg", ["Real"],"Real"),
                ("Eq", ["Real", "Real"], "Bool"),
                ("LEq", ["Real", "Real"], "Bool")
    ])
--Natural nums
nat :: Signature 
nat =(["Nat","Bool"],[
                ("Add", ["Nat","Nat"],"Nat"),
                ("Mult", ["Nat","Nat"], "Nat"),
                ("Eq", ["Nat", "Nat"], "Bool"),
                ("LEq", ["Nat", "Nat"], "Bool")
    ])
--Natural nums
bool :: Signature 
bool =(["Bool"],[
            ("Add", ["Bool","Bool"],"Bool"),
            ("Mult", ["Bool","Bool"], "Bool"),
            ("Eq", ["Bool", "Bool"], "Bool"),
            ("LEq", ["Bool", "Bool"], "Bool")
        ])

-- | The semantic domain for the rings sigs
data Ring = BoolDom Bool
            | NatDom Int Bool
            | IntDom Int Bool
            | RealDom Float Bool
            deriving(Show, Eq, Ord)

unpackInt :: Ring->Int
unpackInt (IntDom i _) = i
unpackNat :: Ring->Int
unpackNat (NatDom n _) = n
unpackReal :: Ring->Float
unpackReal (RealDom x _) = x
unpackBool :: Ring->Bool
unpackBool (BoolDom q) = q

getBool ::Ring->Bool
getBool (IntDom _ q) = q
getBool (NatDom _ q) = q
getBool (RealDom _ q) = q
getBool (BoolDom q) = q


-- | Semantics 

--Semantics of a ring(when carrier set is a int)
intRingModel ::FunModel Ring
intRingModel "Add" arg =  IntDom (foldl (+) 0 (map unpackInt arg)) False
intRingModel "Mult" arg = IntDom (foldl (*) 1 (map unpackInt arg)) False
intRingModel "Neg" [x] = IntDom (negate (unpackInt x)) $ getBool x
intRingModel "Eq" [x, y] = BoolDom (x==y)
intRingModel "LEq" [x, y] = BoolDom (x<=y)

--Semantics of a ring(when carrier set is a nat)
natRingModel ::FunModel Ring
natRingModel "Add" arg = NatDom  (foldl (+) 0 (map unpackNat arg)) False
natRingModel "Mult" arg = NatDom (foldl (*) 1 (map unpackNat arg)) False
natRingModel "Neg" [x] = NatDom (negate (unpackNat x)) $ getBool x
natRingModel "Eq" [x, y] = BoolDom (x==y)
natRingModel "LEq" [x, y] =BoolDom (x<=y)

--Semantics of a ring(when carrier set is a real)
realRingModel ::FunModel Ring
realRingModel "Add" arg =  RealDom (foldl (+) 0 ((map unpackReal arg))) False
realRingModel "Mult" arg = RealDom (foldl (*) 1 ((map unpackReal arg))) False
realRingModel "Neg" [x] = RealDom (negate (unpackReal x))  $ getBool x
realRingModel "Eq" [x, y] = BoolDom (x==y)
realRingModel "LEq" [x, y] =BoolDom (x<=y)

--Semantics of a ring(when carrier set is bool)
boolRingModel ::FunModel Ring
boolRingModel "Add" arg =  BoolDom $ foldl (/=) False (map unpackBool arg)
boolRingModel "Mult" arg = BoolDom $ foldl (&&) True (map unpackBool arg)
boolRingModel "Neg" [x] = BoolDom (not (unpackBool x))
boolRingModel "Eq" [x, y] = BoolDom (x==y)
boolRingModel "LEq" [x, y] =BoolDom (x<=y)


-- | Eval
eval ::FunModel Ring->Signature->Enviroment Ring->ExprAST Ring->Ring
eval mod sig env (Lit i _) = i
eval mod sig env (Var name) = case lookup name env of
                            Just x -> eval mod sig env x
                            Nothing -> error "No such var in env"

eval mod (types, funcs) env (Fun name args) | elem name (map (\(x,y,z)->x) funcs) =mod name (map (eval mod (types, funcs) env) args)
                                            | otherwise= error "Can't find function in signature"

eval mod (sig) env (Assert expr) = let res@(BoolDom b) = eval mod sig env expr in if b then res else error $"Assert did not hold"++ (show expr)


-- | Tests that carrier set is abelian group
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

-- | main
test tpname (sig, model) dta = let env = [("a", Lit (dta!!0) tpname), ("b", Lit (dta!!1) tpname), ("c", Lit (dta!!2) tpname), ("multConst", Lit (dta!!3) tpname), ("addConst", Lit (dta!!2) tpname)] in
    map (eval model sig env) tests