{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Calc where

import qualified Data.Map as M
import Data.Maybe
import ExprT
import Parser
import StackVM (Program, StackExp (Add, Mul, PushI))

-- Expressions

{-
Extensive focus group analysis has revealed that what people really want out of their calculator is something that can add and multiply integers. Anything more just clutters the interface.
-}

-- Exercise 1
-- Write Version 1 of the calculator: an evaluator for ExprT
eval :: ExprT -> Integer
eval (ExprT.Lit intNum) = intNum
eval (ExprT.Add expA expB) = eval expA + eval expB
eval (ExprT.Mul expA expB) = eval expA * eval expB

-- Exercise 2
{-
Leverage the assets of the UI team to implement the value-added function
evaluates arithmetic expressions given as a String, producing Nothing for inputs which are not well-formed expressions, and Just n for well-formed inputs that evaluate to n.
-}
evalStr :: String -> Maybe Integer
evalStr exp = case parse exp of
  Just e -> Just (eval e)
  Nothing -> Nothing
  where
    parse = parseExp Lit ExprT.Add ExprT.Mul

-- Exercise 3
{-
The problem the software department (i.e. you) has is that while ExprT is nice, it is also rather inflexible, which makes catering to diverse demographics a bit clumsy.
You decide to abstract away the properties of ExprT with a type class.

Create a type class called Expr with three methods called lit, add, and mul which parallel the constructors of ExprT.
Make an instance of Expr for the ExprT type, in such a way that
    mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
        == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-}
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
-- Integer
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

-- Bool
instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

-- MinMax
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

-- Mod7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

-- Exercise 5
{-
takes Strings representing arithmetic expressions and compiles them into programs that can be run on the custom CPU.
-}
instance Expr StackVM.Program where
  lit a = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
{-
Some users of your calculator have requested the ability to give names to intermediate values and then reuse these stored values later.
To enable this, you first need to give arithmetic expressions the ability to contain variables.

-}
class HasVars a where
  var :: String -> a

data VarExprT = VarExprT String Integer deriving (Eq, Show)

instance Expr VarExprT where
  lit = VarExprT ""
  add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a + b)
  mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)

instance HasVars VarExprT where
  var str = VarExprT str 0

type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars MapExpr where
  var = M.lookup

instance Expr MapExpr where
  lit a = \_ -> Just a
  add a b = \x ->
    if isNothing (a x) || isNothing (b x)
      then Nothing
      else Just (fromJust (a x) + fromJust (b x))
  mul a b = \x ->
    if isNothing (a x) || isNothing (b x)
      then Nothing
      else Just (fromJust (a x) * fromJust (b x))