{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import qualified Data.List as L
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show




------------------------------------------------------------
-- Exercise 2

rollNDies :: Int -> Rand StdGen [DieValue]
rollNDies 0 = return []
rollNDies n = die >>= \x -> rollNDies (n-1) >>= \xs -> return (reverse . L.sort $ (x:xs))


battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield 1 _) = return bf
battle (Battlefield a d) = rollNDies a' >>= \attackers -> 
                           rollNDies d' >>= \defenders ->
                           let (new_a, new_d) = L.foldl' (\(a, d)  
                                                           (sa, sb) ->  if sa > sb then (a, d-1) else (a-1, d))
                                                         (a, d)
                                                         (zip attackers defenders)
                            in return (Battlefield new_a new_d)
  where a' = min (a-1) 3
        d' = min d 2


------------------------------------------------------------
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d) | a < 3 || d == 0 = return bf
                            | otherwise = battle bf >>= invade


------------------------------------------------------------
-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb bf = simulation >>= \res -> return $  (fromIntegral . length . filter (==0) . map defenders $ res) / 1000.0
  where simulation = replicateM 1000 (invade bf)

