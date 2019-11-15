toDigits :: Integer -> [Integer]
toDigits x
          | x <= 0    = []
          | otherwise = map (read . (\a -> [a])) $ show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther x = concat [(doubleEveryOther $ take (length x - 2) x), [(*2) $ last $ init $ x], [last x]]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits  (x:xs) = sum (toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate x = 
        if (rem (sumDigits $ doubleEveryOther $ toDigits x) (10) == 0) 
          then True
          else False


type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = [(x, y)]
hanoi n x y z = concat [hanoi (n - 1) x z y, hanoi 1 x y z, hanoi (n - 1) z y x]

hanoi' :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 x y z w = hanoi 1 x y z
hanoi' 2 x y z w = [(x, z), (x, y), (z, y)]
hanoi' n x y z w = concat [hanoi' (n - 2) x w y z, hanoi' 2 x y z w, hanoi' (n - 2) w y x z]
