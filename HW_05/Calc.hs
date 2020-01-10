import ExprT
import Parser

-- module Calc where

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + (eval y)
eval (Mul x y) = eval x * (eval y)

evalStr :: String -> Maybe Integer
evalStr x = case (Parser.parseExp Lit Add Mul x) of 
                    Just y -> Just (eval y)
                    Nothing -> Nothing
