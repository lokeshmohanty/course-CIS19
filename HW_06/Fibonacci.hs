{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

-- #----------------------------#
-- #------- Exercise 1 ---------#
-- #----------------------------#

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = foldr (\x -> \y -> fib x : y) [] [0..]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

fibs1' :: [Integer]
fibs1' = foldr' (\x -> \y -> fib x : y) [] [0..]

-- #----------------------------#
-- #------- Exercise 2 ---------#
-- #----------------------------#

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
-- fibs2 =  fibs2 (\xs -> xs ++ [(last xs + (last $ init xs))]) [0,1]
-- fibs2 = foldr (\_ xs -> xs ++ [(last $ init xs) + (last xs)]) [0,1] [1..]


-- #----------------------------#
-- #------- Exercise 3 ---------#
-- #----------------------------#

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x y) = x:(streamToList y)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- #----------------------------#
-- #------- Exercise 4 ---------#
-- #----------------------------#

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f $ f x

-- #----------------------------#
-- #------- Exercise 5 ---------#
-- #----------------------------#

nats :: Stream Integer
nats = streamFromSeed (+1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x $ interleaveStreams ys xs
-- the below function cause stack overflow when ruler' uses it
-- interleaveStreams (Stream x xs) (Stream y ys) = 
--   Stream x $ Stream y $ interleaveStreams xs ys

ruler :: Stream Integer
ruler = streamMap maxDivBy2 nats
  where maxDivBy2 0 = 0
        maxDivBy2 x = case (rem x 2) of
                        0 -> maxDivBy2 (div x 2) + 1
                        _ -> 0

ruler' = foldr1 interleaveStreams $ map streamRepeat [0..]
-- ruler' = foldr1 interleaveStreams $ streamRepeat <$> [0..]


-- #----------------------------#
-- #------- Exercise 6 ---------#
-- #----------------------------#

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = Stream x $ streamRepeat 0
  -- negate (Stream x xs) = Stream (negate x) (negate xs)
  negate = streamMap negate
  (+) (Stream x xs) (Stream y ys) = Stream (x + y) $ (+) xs ys
  (*) (Stream x xs) b@(Stream y ys) = Stream (x * y) $ (streamMap (*x) ys) + (b * xs)
  -- (*) (Stream x xs) b@(Stream y ys) = Stream (x * y) $ (fromInteger x * ys) + (b * xs)

instance Fractional (Stream Integer) where
  -- (/) a@(Stream x xs) b@(Stream y ys) = Stream (div x y) $ streamMap (`div` y) $ (-) xs $ ys * (a / b)
  (/) a@(Stream x xs) b@(Stream y ys) = q
    where q = Stream (div x y) $ streamMap (`div` y) $ xs - ys * q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

-- #----------------------------#
-- #------- Exercise 7 ---------#
-- #----------------------------#

-- type Matrix = (Integer, Integer, Integer, Integer)
--
-- instance Num Matrix where
--   negate (x,y,z,w) = (n x,n y,n z,n w) where n = negate
--   (+) (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1 + y1, x2 + y2, x3 + y3, x4 + y4)
--   (*) (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1*y1 + x2*y3, x1*y2 + x2*y4, x3*y1 + x4*y3, x3*y2 + x4*y4)


data Matrix = Matrix { a00 :: Integer
                     , a10 :: Integer
                     , a01 :: Integer
                     , a11 :: Integer
                     } deriving (Show, Eq)

instance Num Matrix where
  fromInteger n = Matrix n n n n
  negate (Matrix x y z w) = Matrix (-x) (-y) (-z) (-w)
  (+) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) = Matrix (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)
  (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) = Matrix (x1*y1 + x2*y3) (x1*y2 + x2*y4) (x3*y1 + x4*y3) (x3*y2 + x4*y4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 x = a10 $ Matrix 1 1 1 0 ^ x

