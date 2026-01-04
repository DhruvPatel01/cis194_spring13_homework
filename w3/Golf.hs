module Golf where

dropAndTake [] n = []
dropAndTake xs n = case drop n xs of
  [] -> []
  x : xs -> x : dropAndTake xs n

skips :: [a] -> [[a]]
skips s = map (dropAndTake s) [0 .. length s - 1]

localMaxima lst = [y | (x, y, z) <- zip3 lst (drop 1 lst) (drop 2 lst), x < y && z < y]

count x = length . filter (== x)

histogram xs = unlines $ strHist ++ ["==========", ['0'..'9']]
  where
    intHist = map (`count` xs) [0 .. 9]
    strHist = reverse [[if x >= t then '*' else ' ' | x <- intHist ] | t <- [1..maximum intHist]]
