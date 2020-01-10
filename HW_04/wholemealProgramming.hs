-- File: HW_04/wholemealProgramming.hs

-- Exercise 1: Wholemeal programming
fun1 = product . map (subtract 2) . filter even
fun2 = sum . filter even . takeWhile (/=1) . iterate (\x -> if (even x) then (div x 2) else (3 * x + 1))

-- Exercise 2: Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr addToTree Leaf

addToTree :: (Ord a) => a -> Tree a -> Tree a
addToTree y Leaf = Node 0 Leaf y Leaf
addToTree y x@(Node h left root right)
  |  left > right = Node (height newRight + 1) left root newRight
  | otherwise    = Node (height newLeft + 1) newLeft root right
    where 
      newRight = addToTree y right
      newLeft  = addToTree y left 

height :: Tree a -> Integer
height Leaf = -1
height (Node h x y z) = h

-- Exercise 3: More folds!

xor :: [Bool] -> Bool
xor = foldr (\a -> \b -> if (a /= b) then True else False) False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> \b -> f a : b) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base xs = foldr (flip f) base $ reverse xs

-- Exercise 4: Finding primes

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], not $ elem x (composite n)] 

composite :: Integer -> [Integer]
composite n = removeDuplicate $ sort [i + j + 2*i*j | i <- [1..n], j <-[1..n]];

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = concat [(sort [i | i <- xs, i < x]), [x], (sort [i | i <- xs, i > x])]

removeDuplicate :: (Eq a) => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs) = x : (removeDuplicate $ dropWhile (\y -> y == x) xs)

-- TODO: Find Primes by Riemann Hypothesis

