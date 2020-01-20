{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M


-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + (eval y)
eval (ExprT.Mul x y) = eval x * (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr x = case (Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul x) of 
                    Just y -> Just (eval y)
                    Nothing -> Nothing

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = ExprT.Lit x
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x == True || y == True
  mul x y = x == True && y == True

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul 

testInteger x = testExp x :: Maybe Integer
testBool x = testExp x :: Maybe Bool
testMM x = testExp x :: Maybe MinMax
testSat x = testExp x :: Maybe Mod7

-- Exercise 5
instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = case (x, y) of 
              (PushI xi : xs, PushI yi : ys) -> concat [x, y, [StackVM.Add]]
              (PushB xb : xs, PushB yb : ys) -> concat [x, y, [StackVM.Or]]
  mul x y = case (x, y) of 
              (PushI xi : xs, PushI yi : ys) -> concat [x, y, [StackVM.Mul]]
              (PushB xb : xs, PushB yb : ys) -> concat [x, y, [StackVM.And]]

compile :: String -> Maybe Program
compile x = testExp x :: Maybe Program

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer | Add VarExprT VarExprT | Mul VarExprT VarExprT | Var String deriving (Show, Eq)

instance Expr VarExprT where
  lit x = Calc.Lit x
  add x y = Calc.Add x y
  mul x y = Calc.Mul x y

instance HasVars VarExprT where
  var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \m -> Just x
  add x y = \m -> case (x m, y m) of
                    (Just x1, Just y1) -> Just $ x1 + y1
                    otherwise          -> Nothing
  mul x y = \m -> case (x m, y m) of
                    (Just x1, Just y1) -> Just $ x1 * y1
                    otherwise          -> Nothing

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

