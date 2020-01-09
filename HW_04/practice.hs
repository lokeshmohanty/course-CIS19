-- file: HW_04/practice.hs

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

-- promise, thunk
-- cycle, repeat, replicate, elem, zip
-- List Comprehension: [x*2 | x <- [1..10]] -- Similar to Sets in Maths
-- List Comprehension with predicate: [x*2 | x <- [1..10], x*2 >= 12]

main :: IO ()
main = do
       print [x*2 | x <- [2..100], x /= 14, x /= 20, x /= 98]
       print [x*y | x <- [1..5], y <- [5..9], mod x 2 == 1]

length' xs = sum [1 | _ <- xs]

filterOutOdd xxs = [(\xs -> [x | x <- xs, mod x 2 == 0]) xs | xs <- xxs]
filterOutOdd' xxs = [[x | x <- xs, mod x 2 == 0] | xs <- xxs]

triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a + b + c == 24]

rightTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a + b + c == 24, a^2 + b^2 - c^2 == 0]


-- Higher Order functions
-- map, filter, foldl, foldr, foldr1, foldl1, scanl, scanr, scanl1, scanr1
