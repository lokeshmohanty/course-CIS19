module Golf where

-- skips :: [a] -> [[a]]
skips x = concat [f (i-1) x | i <- [1..(length x)]]

f 0 x = [map (x !!) [0..(length x - 1)]]
f i x = [map (x !!) [i,(2*i)..(length x - 1)]]

g :: Ord a => [a] -> Int -> [a]
g x 0 = []
g x n = if (x !! n >= (x !! (n - 1)) && x !! n >= (x !! (n + 1)))
          then concat [g x (n-1), [x !! n]]
          else g x (n - 1)

localMaxima :: Ord a => [a] -> [a]
localMaxima x = g x (length x - 2)

count x n = sum [(\x -> \y -> if (x == y) then 1 else 0) n i | i <- x]
countHistogram x = [count x i | i <- [1..9]]

star x = reverse $ [func x i | i <- [1..(maximum x)]]
func x y = map ((\n -> \m -> if (n <= m) then '*' else ' ') y) x

histogram :: [Integer] -> String
histogram x = unlines $ concat [star (countHistogram x), ["==========", "123456789"]]
