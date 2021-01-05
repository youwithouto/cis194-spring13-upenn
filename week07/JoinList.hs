{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

-- Editors and Buffers
data JoinListBasic a = Empty
                    | Single a
                    | Append (JoinListBasic a) (JoinListBasic a)

-- Monoidally Annotated Join-Lists
-- The idea is that the annotation at the root of a JoinList will always be equal to the combination of all the annotations on the Single nodes (according to whatever notion of “combining” is defined for the monoid in question)
-- Empty nodes do not explicitly store an annotation, but we consider them to have an annotation of mempty (that is, the identity element for the given monoid).
data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- Exercise 2
-- The first annotation to try out is one for fast indexing into a JoinList. 
-- The idea is to cache the size (number of data elements) of each subtree.
-- This can then be used at each step to determine if the desired index is in the left or the right branch.

-- 1
-- indexJ finds the JoinList element at the specified index.
-- If the index is out of bounds, the function returns Nothing.
-- By an index in a JoinList we mean the index in the list that it represents.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i jl | i >= getSize jl = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ jl1 jl2)
    | i < left  = indexJ i jl1
    | otherwise = indexJ (i - left) jl2
    where left = getSize jl1

-- Get the size of a given JoinList
getSize :: (Monoid b, Sized b) => JoinList b a -> Int
getSize = getSize . size . tag 

-- Convert a JoinList to a list
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Get the ith element from a list
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0 =Justx (x:xs) !!? i = xs !!? (i-1)


-- 2
-- The dropJ function drops the first n elements from a JoinList.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n < 0 = jl
dropJ n jl | n >= getSize jl = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ jl1 jl2)
    | n < left = dropJ n jl1 +++ jl2
    | otherwise = dropJ (n - left) jl2
    where left = getSize jl1

-- 3
-- The takeJ function returns the first n elements of a JoinList, dropping all other elements.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n < 0 = Empty
takeJ n jl | n >= getSize jl = jl
takeJ _ jl@(Single _ _) = jl
dropJ n (Append _ jl1 jl2)
    | n < left = takeJ n jl1
    | otherwise = jl1 ++ takeJ (n - left) jl2
    where left = getSize jl1


-- Exercise 4
instance Monoid m => Monoid (JoinList m a) where
    mempty  = Empty
    mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
    toString          = unlines . jlToList
    fromString        = mconcat . fmap createList . lines
        where createList s = Single (scoreString s, Size 1) s
    line              = indexJ
    replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b
    numLines          = getSize
    value             = getScore . fst . tag


reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id

main :: IO()
main = runEditor editor . reify . fromString $ unlines
    [
        "Hello, buffer"
    ]