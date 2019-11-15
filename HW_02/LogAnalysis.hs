{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage (x:xs) 
  |x == 'I' = LogMessage Info (read $ head $ words xs) (unwords $ tail$ words xs)
  |x == 'W' = LogMessage Warning (read $ head $ words xs) (unwords $ tail$ words xs)

  |x == 'E' = LogMessage (Error (read $ words xs !! 0)) (read $ head $ tail $ words xs) (unwords $ drop 2 $ words xs)
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x


-- Exercise 2

getTimestamp :: LogMessage -> Maybe Int
getTimestamp (LogMessage _ x _) = Just x
getTimestamp (Unknown _) = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) n = n
insert x Leaf = Node Leaf x Leaf
insert x (Node nl y nr) = if (getTimestamp x > getTimestamp y)  
                            then Node nl y (insert x nr)
                            else Node (insert x nl) y nr

build :: [LogMessage] -> MessageTree
build [] = Leaf
-- build x = insert (head x) (build (tail x))
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node nl x nr) = concat [inOrder nl, [x], inOrder nr]

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ((LogMessage (Error e) _ y):xs) = if (e >= 50) then concat [[y], whatWentWrong xs] else whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs
whatWentWrong _ = []

info :: [LogMessage] -> [String]
info ((LogMessage I _ y):xs) = concat [[y], whatWentWrong xs]
info (_:xs) = whatWentWrong xs
info _ = []
