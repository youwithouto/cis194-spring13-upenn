module Party where

import Data.List
import Data.Tree
import Employee

-- Exercise 1
-- 1
-- Adds an Employee to the GuestList (updating the cached Fun score appropriately)
-- glCons should simply add the new Employee and add their fun score without doing anykind of checks
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

-- 2
-- A Monoid instance for GuestList
instance Monoid GuestList where
  mempty = GL [] 0
  GL es f1 `mappend` GL fs f2 = GL (es ++ fs) (f1 + f2)

-- 3
-- Takes two GuestLists and returns whichever one of them is more fun, i.e., has the higher fun score
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a sf) = f a (treeFold f <$> sf)

-- Exercise 3
-- Takes two arguments:
-- The first is the "boss" of the current subtree; the second argument is a list of the results for each subtree under the boss.
-- Each result is a pair of GuestLists: the first GuestList in the pair is the best possible guest list with the boss of that subtree; the second is the best possible guest list without the boss of that subtree.
-- nextLevel should then compute the overall best guest list that inludes the boss, and the overall guest list that doesn't include the boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss withoutBosses, withBosses)
  where
    withoutBosses = foldMap fst gls
    withBosses = foldMap snd gls

-- Exercise 4
-- Takes a company hierarchy as input and outputs a fun-maximizing guest list.
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
formatEmployee :: [Employee] -> String
formatEmployee = unlines . sort . fmap empName

formatGuestList :: GuestList -> String
formatGuestList (GL es fun) = "Total fun: " ++ show fun ++ "\n" ++ formatEmployee es

main :: IO ()
main = do
  contents <- readFile "company.txt"
  putStr . formatGuestList . maxFun . read $ contents