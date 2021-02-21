# Homework 5



## Expressions

Background:

- Program the brains of the company's new blockbuster product: a calculator

Target:

- Add and multiply integers

Definitions:

```haskell
data ExprT = Lit Integer
					 | Add ExprT ExprT
					 | Mul ExprT ExprT
           deriving (Show, Eq)
```





### Exercise 1

Target:

- Write an evaluator for `ExprT`

Definition:

```haskell
eval :: ExprT –> Integer
```

Solution:

```haskell
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expA expB) = eval expA + eval expB
eval (Mul expA expB) = eval expA * eval expB
```

- Pattern match on the value constructors of `ExprT`



### Exercise 2

Defintion:

```haskell
evalStr :: String -> Maybe Integer
```

Solution:

```Haskell
evalStr :: String -> Maybe Integer
evalStr exp = (maybe Nothing $ Just . eval) . parseExp Lit Add Mul exp
evalStr = (maybe Nothing $ Just . eval) . parseExp Lit Add Mul
evalStr = fmap eval . parseExp Lit Add Mul
```

Related Haskell Concepts:

- Functor



### Exercise 3

Target:

- Create a type class

Solution:

```haskell
class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a
	
instance Expr ExprT where
	lit = Lit
	add = Add
	mul = Mul
	
--
reify :: ExprT -> ExprT
reify = id
```



### Exercise 4

Target:

- Make instances of Expr for each of the following types:
  - Integer
  - Bool
  - MinMax
  - Mod7

Definitions:

```haskell
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)
```

Examples:

```haskell
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM   	  = testExp :: Maybe MinMax
testSat 		= testExp :: Maybe Mod7
```

Solution:

```Haskell
instance Expr Integer where
	lit = id
	add = (+)
	mul = (*)
	
instance Expr Bool where
	lit = (<= 0)
	add = (||)
	mul = (&&)
	


newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
	lit 											= MinMax
	add (MinMax a) (MinMax b) = MinMax $ max x y
	mul (MinMax a) (MinMax b) = MinMax $ min x y


newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
	lit n = Mod7 $ n `mod` 7
	add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
	mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
```



### Exercise 5

Target:

- Implement another calculator using a custom stack-based CPU
  - Create an instance of the Expr type case for Program

Examples:

```Haskell
instance Expr Program where
	lit a   = [PushI a]
	add a b = a ++ b ++ [Add]
	mul a b = a ++ b ++ [Mul]
```

- Since the solution needs to run on a stack machine, its operation should be pushed onto the stack

```haskell
compile :: String -> Maybe Program
compile = parseExp lit add mul
```



### Exercise 6

Target:

- Handle variables

Definitions:

```Haskell
class HasVars a where
	var :: String -> a
```



Solution:

```Haskell
data VarExprT =
	| Lit Integer
	| Var String
	| Add VarExprT VarExprT
	| Mul VarExprT VarExprT
	deriving (Show, Eq)
	
instance Expr VarExprT where
	lit = Lit
	add = add
	mul = mul
	
instance HasVars VarExprT where
	var = Var
	
--
instance Expr (M.Map String Integer -> Maybe Integer) where
	lit a _ = Just a
	add a b m = (+) <$> a m <*> b m
	mul a b m = (*) <$> a m <*> b m


instance HasVars (M.Map String Integer -> Maybe Integer) where
	var = M.lookup



```



