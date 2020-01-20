{-# LANGUAGE FlexibleInstances #-}
data Foo = F Int | G Char

-- First Way
-- instance Eq Foo where 
--   (F i1) == (F i2) = i1 == i2
--   (G c1) == (G c2) = c1 == c2
--   _ = False
--   foo1 /= foo2 = not (foo1 == foo2)
--

-- Second Way
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x /= y = not (x == y)

-- Best Way
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)


class Listable a where
  toList :: a -> [Int]

-- toList :: Listable a => a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  -- toList :: Bool -> [Int]
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)
  
instance Listable (Tree Int) where
 toList Empty = []
 toList (Node x y z) = concat [toList y, [x], toList z]

sumL :: Listable a => a -> Int
sumL = sum . toList

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum . toList x == sum . toList y || x < y
