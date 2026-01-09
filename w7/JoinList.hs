{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid

import Sized
import Scrabble
import Buffer
import Editor

import qualified Data.List as L

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)




(+++) :: Monoid m => JoinList m  a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) x Empty =  x
(+++) Empty x = x
(+++) a b = Append (tag a <> tag b) a b



tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m 
tag Empty = mempty


----------------------
---- EXC 2
----------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Single _ _) = Nothing
indexJ i (Append m l r) | i >= (getSize. size) m = Nothing
                        | i < (getSize. size . tag)  l = indexJ i l
                        | otherwise = indexJ (i - (getSize.size.tag $ l)) r


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i e | i <= 0 = e
dropJ n r | n >= (getSize.size.tag) r = Empty
dropJ i (Append m l r) | i <= (getSize.size.tag) l = (dropJ i l) +++ r
                       | otherwise =  dropJ (i - (getSize.size.tag) l) r
dropJ _ _ = Empty


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ e@(Single m a) = e
takeJ n (Append m l r) | n == (getSize.size.tag) l = l
                       | n < (getSize.size.tag) l = takeJ n l
                       | otherwise =  l +++ (takeJ (n-(getSize.size.tag) l) r)
                        


testTree = Append (Size 4)
            (Append (Size 3)
            (Single (Size 1) 'y')
            (Append (Size 2)
            (Single (Size 1) 'e')
            (Single (Size 1) 'a')))
            (Single (Size 1) 'h')


----------------------
---- EXC 3
----------------------

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

----------------------
---- EXC 4
----------------------
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = L.foldl' (+++) Empty . map (\l -> Single (scoreString l, 1) l). lines
  line   = indexJ
  numLines = getSize.size.tag
  value = getScore.fst.tag
  replaceLine n l b | n >= (getSize.size.tag) b = b
                    | otherwise = (takeJ n b) +++ (Single (scoreString l, 1) l) +++ (dropJ (n+1) b)


main = runEditor editor (Empty :: JoinList (Score, Size) String)
