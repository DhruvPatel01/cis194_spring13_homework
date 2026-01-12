-- {-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Semigroup
import Data.Tree
import qualified Data.List as L




-----------------
--- Exc 1
-----------------

glCons:: Employee -> GuestList -> GuestList
glCons e@(Emp _ funScore) (GL emps totalFunScore) = GL (e:emps) (funScore + totalFunScore)


instance Semigroup GuestList where
    (<>) (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1+f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b


-----------------
--- Exc 2
-----------------

-- Based on the function type from week 7 notes
-- function should take label of current root, and a list of results of children

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node label forest) = f label (map (treeFold e f) forest) 


-----------------
--- Exc 3
-----------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subResult = (withBoss, withoutBoss)
    where
        withBoss = glCons boss (L.foldl' (<>) mempty (map snd subResult))
        withoutBoss = L.foldl' (<>) mempty (map (uncurry moreFun) subResult) 


-----------------
--- Exc 4
-----------------

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withCEO withoutCEO
    where
        (withCEO, withoutCEO) = treeFold (mempty, mempty)  nextLevel tree


-----------------
--- Exc 5
-----------------


main = do
    company <- readFile "company.txt"
    let GL emps fun = maxFun (read company)
        names = unlines $ L.sort (map empName emps)
    putStrLn $ "Total fun: " ++ (show fun)
    putStr names
