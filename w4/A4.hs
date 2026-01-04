module A4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

remainingElements n xs = concat $ zipWith (\l r -> [l + 1 .. r - 1]) fxs (tail fxs)
  where
    fxs = [0] ++ xs ++ [n + 1]

sundaram n = map (\n -> 2 * n + 1) . remainingElements n . sort . concat $ [[3 * a + 1, 5 * a + 2 .. n] | a <- [1 .. n]]

xor :: [Bool] -> Bool
xor = foldl' (\b a -> if a then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

----

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

getHeight Leaf = 0
getHeight (Node h _ _ _) = h

addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 1 Leaf x Leaf
addNode x (Node h l c r)
  | lh < rh = Node h (addNode x l) c r
  | lh > rh = Node h l c (addNode x r)
  | otherwise = let newLeft = addNode x l in Node ((+ 1) . maximum . map getHeight $ [newLeft, r]) newLeft c r
  where
    lh = getHeight l
    rh = getHeight r

isBalanced Leaf = True
isBalanced (Node _ l _ r) = (diff >= -1 || diff <= 1) && isBalanced l && isBalanced r
  where
    diff = getHeight l - getHeight r