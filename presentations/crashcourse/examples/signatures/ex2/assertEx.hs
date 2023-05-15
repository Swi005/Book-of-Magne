module AssertEx where

import AST
import Integers hiding (FunModel)
import Naturals hiding (FunModel)
import Booleans hiding (FunModel)


type FunName = String

-- | f a b = f b a
commutative :: FunName -> [Expr valueDomain] -> FunModel valueDomain ->SimpleEnviroment valueDomain
commutative f args fmod = exec fmod [] (AssertEq (FunCall f args) (FunCall f (reverse args)))

-- | f a (inverse a) = 0
addInverse :: FunName -> FunName -> [Expr valueDomain] -> FunModel valueDomain -> SimpleEnviroment valueDomain
addInverse f inverse args fmod = exec fmod [] (AssertEq (FunCall f args) (FunCall f (eval fmod [] (FunCall inverse args))))

-- | f a neutral = a
identity :: FunName -> [Expr valuedomain] -> FunModel valueDomain -> SimpleEnviroment valueDomain
identity f [a, neutral] fmod = exec fmod [] (AssertEq (FunCall f [a, neutral]) (Lit a))

-- | f (f a b) c = f a (f b c)
associative :: FunName -> [Expr valueDomain] -> FunModel valueDomain -> SimpleEnviroment valueDomain
associative f [a,b,c] fmod = exec fmod [] (AssertEq (FunCall f [FunCall f [a,b], c]) (FunCall f [a, FunCall f [b,c]]))

-- | f a (g b c) = f (g a b) (g a c)
distributive :: FunName -> FunName -> [Expr valueDomain] -> FunModel valueDomain -> SimpleEnviroment valueDomain
distributive add mult [a,b,c]  fmod = exec fmod [](AssertEq (FunCall mult [a, FunCall add [b,c]]) (FunCall add [FunCall mult [a,b], FunCall mult [a,c]]))

