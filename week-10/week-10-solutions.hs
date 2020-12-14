-- Add element to the start of a list
-- 4:[1,2,3] -> [4,1,2,3]
-- collections have types guys, you can't mix/match what's inside
-- [1,2] ++ [3,4] -> [1,2,3,4]
-- Check if in list
-- elem 1 [1,2,3] -> True
-- elem 0 [1,2,3] -> False


-- Exercise: Write a the drop. Use. Pattern. Matching. Please.

drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs


-- Ok, we have all the higher order functions. The usual suspects.

-- map id [1,2,3]
-- filter even [1,2,3]
-- the folds have two variants:
-- foldr (+) 0 [1,2,3] <-- with initial argument
-- foldr1 (+) [1,2,3] <-- with first item as initial argument
-- same for foldl.

-- Finally, we also have list comprehension(traversal on steroids)
-- [x | x <- [1,2,3], even x]
-- Read it as "for X from BLA where CONDITION_IS_TRUE"

-- Exercise: Generalize our old isPrefix to a fuinction checking if a string contains another

-- We had isPrefix once upion a time
-- I will fix it before submitting to GH.
isPrefix :: String -> String -> Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys


strContains :: String -> String -> Bool
strContains _ [] = True
strContains [] _ = False
strContains xs ys
  | isPrefix ys xs = True
  | strContains (tail xs) ys = True
  | otherwise = False


-- Let's briefly discuss typeclasses.
-- tl;dr - Interfaces++. Additional type safety at compile time + behavior.
-- A boring max fn

myMaxInt :: Int -> Int -> Int
myMaxInt n m
   | n > m = n
   | otherwise = m

myMax :: (Ord a) => a -> a -> a
myMax n m
  | n > m = n
  | otherwise = m

-- And we are also guaranteed that the A is the same A!


-- Let's do a sort. The best one(to pretend you are smart, that is)

-- Merge two already ordered collections:

myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys)
  | x <= y = x:myMerge xs (y:ys)
  | otherwise = y:myMerge (x:xs) ys


-- What is mergesort?
--You merge recursively the halfs of the mergesort of the collection you have
-- ^^ Enough to implement

myMergeSort :: (Ord a) => [a] -> [a]
myMergeSort xs
  | len < 2 = xs
  | otherwise = myMerge (myMergeSort l) (myMergeSort r) where
      len = length xs
      (l, r) = splitAt (div len 2) xs
