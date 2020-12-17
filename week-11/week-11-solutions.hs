-- What did we do in the lectures?
-- Interesting stuff, but we're ahead of it!

-- Exercise 1: Every positive even number greater than 2 is the sum of two primes. Find 'em.

-- First. how do we find a prime?

isPrime :: Int -> Bool
isPrime n = length [ x | x <- [2..(div n 2)], mod n x == 0] == 0

primeDecompose n = head $ [(x, n - x) | x <- [3..n `div` 2], isPrime x && isPrime (n - x)]




-- Exercise 2: Let's do something cooler again. What is a Burrows Wheeler transform?
-- Let's do a naive one
-- https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform

-- Can we rotate?

rot :: String -> Int -> String
rot word n = drop n word ++ take n word

rotations :: String -> [String]
rotations word = [rot word x | x <- [1..(length word)]]

-- The old mergesort from last time:

myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys)
  | x <= y = x:myMerge xs (y:ys)
  | otherwise = y:myMerge (x:xs) ys


myMergeSort :: (Ord a) => [a] -> [a]
myMergeSort xs
  | len < 2 = xs
  | otherwise = myMerge (myMergeSort l) (myMergeSort r) where
      len = length xs
      (l, r) = splitAt (div len 2) xs
--

bw :: String -> String
bw s = map last (myMergeSort $ rotations s)


-- Exercise: Solve with fold - get a sequence, sum odd, subtract even

lolSum :: [Int] -> Int
lolSum xs = foldl (\agg n -> if even n then agg - n else agg + n) 0 xs


-- Exercise: Solve the racket exercise "isInAlphabet"

-- Solution 1 - fold + partial app
isInAlphabet :: String -> (String -> Bool)
isInAlphabet alphabet = foldl (\ agg x -> agg && elem x alphabet) True


-- Solution two:
-- dot (.) operator in haskell is function composition.
-- . != $ - $ is just a precedence operator (forces right associativity)
isInAlphabet2 :: String -> (String -> Bool)
isInAlphabet2 alphabet = and . map (\c -> elem c alphabet)
