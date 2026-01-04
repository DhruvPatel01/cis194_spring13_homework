{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExprT
import Parser (parseExp)
import qualified StackVM as VM
import StackVM (Program)

import qualified Data.Map as M
import Data.Maybe

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

---
-- Exercise 2
---

evalStr :: String -> Maybe Integer
evalStr e = fmap eval (parseExp Lit Add Mul e)

---
-- Exercise 3
---

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    mul = ExprT.Mul
    add = ExprT.Add

---
-- Exercise 4
---

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit x = x > 0 
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Ord, Eq, Show)


instance Expr MinMax where
    lit = MinMax 
    mul = min
    add = max

instance Expr Mod7 where
    lit = Mod7
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7) 
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7) 

---
-- Exercise 5
---

instance Expr VM.Program where
    lit x = [VM.PushI x]
    mul x y = x ++ y ++ [VM.Mul]
    add x y = x ++ y ++ [VM.Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul


---
-- Exercise 6
---

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | VVar String
  deriving (Show, Eq)


instance Expr VarExprT where
    lit = VLit
    mul = VMul
    add = VAdd

instance HasVars VarExprT where
    var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup 

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    mul e1 e2 m =if isJust a && isJust b then Just (fromJust a * fromJust b) else Nothing
        where 
            a = e1 m
            b = e2 m
    add e1 e2 m = if isJust a && isJust b then Just (fromJust a + fromJust b) else Nothing
        where 
            a = e1 m
            b = e2 m
