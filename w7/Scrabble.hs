{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Semigroup

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

getScore ::  Score -> Int
getScore (Score x) =  x

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0


score :: Char -> Score
score c | c `elem` "aeilnorstuAEILNORSTU" = Score 1
        | c `elem` "dgDG" = Score 2
        | c `elem` "bcmpBCMP" = Score 3
        | c `elem` "fhvwyFHVWY" = Score 4
        | c `elem` "kK" = Score 5
        | c `elem` "jxJX" = Score 8
        | c `elem` "qzQZ" = Score 10
        | otherwise = Score 0

scoreString :: String -> Score
scoreString = sum . map score 