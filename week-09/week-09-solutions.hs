-- Exercise next week:


-- What is Haskell?
-- *is* Statically Typed => Compile time correctness
-- *is* Strongly Typed => There is no implicit coercing between types.
-- *is* lazy evaluated => Deferring of calculation/work.
-- *is* currying functions by default.
-- What are interfaces? -> Dealing with behaviour
-- *has* Typeclasses -> Interfaces++
-- Has multiple types of polymorphism available
-- G: Google the expression problem
-- *has* List comprehension ->
-- *has* Pattern matching ->
-- Will see ADTs


-- Inheritance:
---- More good than bad - 7
---- More bad than good - 4
-- Could inheritance be bad?
-- G: Composition over inheritance.
-- G: Diamond problem - Google it.


sumTwoNumbers :: Integer -> Integer -> Integer
sumTwoNumbers m n = m + n


-- Write a function that accepts an integer and returns a list of all integers greater than it.

-- allGreaterThan 5 => [5, 6, 7......]

allGreaterThan :: Integer -> [Integer]
allGreaterThan n = n : allGreaterThan (n + 1)


--Exercise: Write a function that counts the digits in an integer.

-- Solution: We take the quotient after division

countAllDigits :: Integer -> Integer
countAllDigits n
  | n < 10 = 1
  | otherwise = 1 + countAllDigits (div n 10)


-- Multiple types of writing code - see guards, pattern matching.

-- What is factorial - solve it with guards or pattern matching.

factorialG :: Integer -> Integer
factorialG n
  | n == 0 = 1
  | otherwise = n * factorialG (n-1)

-- factorialP :: Integer -> Integer
-- factorialP 1 = 1
-- factorialP n = n * factorialP (n-1)


-- What are strings in Haskell?

-- Accepts a string and returns exactly the same string
identityForString :: String -> String
identityForString s = s

identityForStringButLessDumb :: [Char] -> [Char]
identityForStringButLessDumb s = s

-- Write a predicate that accepts two strings and returns true if the 2nd is a prefix of the first
-- Use pattern matching. I don't care if you can solve it with if. Anybody can.


-- Pattern matching -

-- isPrefix "abc" "ab" -> true
-- isPrefix "acb" "ab" -> false

-- isPrefix "abc" "ab" -> true

-- What patterns are we going to match?
-- First is empty, 2nd is not empty = false
-- First is not empty, 2nd is empty = true
-- The rest - both has shit inside =


isPrefix :: String -> String -> Bool
isPrefix _ [] = True
isPrefix [] _ = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
