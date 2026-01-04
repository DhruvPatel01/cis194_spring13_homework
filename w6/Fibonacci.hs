{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..] 

-- fibs2 :: [Integer]
-- fibs2 = map fst (iterate (\(a, b) -> (b, a+b)) (0, 1))

fibs2 :: [Integer]
fibs2 = let fib2 a b = a+b : fib2 b (a+b) in 0:1:fib2 0 1 


-- Ex 3

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x rem) = x: streamToList rem

instance Show a => Show (Stream a) where
    show s = show (take 30 $ streamToList s)

-- Ex 4

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x ys) = Stream (f x) (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed))

-- Ex 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs) 


-- lightbulb: if you take all odd integers, they are only divisible by 1. 
-- The remaining ones are 2 * (1,2,3,...), so the ruler for them is same as the original ruler + 1
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)


-- Ex 6

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)

    negate = streamMap negate

    (+) (Stream x xs) (Stream y ys) = Stream (x+y) (xs + ys) 

    (*) (Stream x xs) b@(Stream y ys) = Stream (x*y) (streamMap (*x) ys + xs*b)

instance Fractional (Stream Integer) where
    (/) a@(Stream x xs) b@(Stream y ys) = Stream (x `div` y)  (streamMap (`div` y) (xs - (a/b*ys)))


fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Ex 7

data Matrix = Matrix Integer Integer Integer  Integer -- row major order

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix p q r s) = Matrix (a*p + b*r) (a*q + b*s) (c*p + d*r) (c*q + d*s) 

fibs4 :: Integer -> Integer
fibs4 n = f where (Matrix _ f _ _) = (Matrix 1 1 1 0)^n
