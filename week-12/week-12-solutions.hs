--What did we discuss on the lecture??

-- Exercise 1: Polymer reaction (Advent of Code 2018). Polymers are expressed as letters:
-- The polymer is formed by smaller units which, when triggered, react with each other such
-- that two adjacent units of the same type and opposite polarity are destroyed. Units' types
-- are represented by letters; units' polarity is represented by capitalization. For instance,
-- r and R are units with the same type but opposite polarity, whereas r and s are entirely
-- different types and do not react.

-- Polymers are formed by aA / bB / cC / dD
-- Where a and A are the same unit with different polarity
-- Example of fully reacting a polymer:

-- dabA|cC|aCBAcCcaDA ->  dab|Aa|CBAcCcaDA -> dabCBA|cC|caDA -> dabCBAcaDA <-- This is the minimal form

import Data.Char
reaction :: Char -> Char  -> Bool
reaction c1 c2 = abs(ord c1 - ord c2) == 32

-- reaction 'a' A

reactionCh :: Char -> String -> Bool
reactionCh _ [] = False
reactionCh c1 psequence = c1 /= c2 && (toUpper c1 == toUpper c2) where c2 = head psequence

-- reactionCh 'a' "A

polymerFold :: String -> String
polymerFold = foldr (\c agg -> if reactionCh c agg then tail agg else c:agg) []


-- Brief discussion about algebraic types:
-- Product types - Many make a whole. (e.g. data Car = ...
-- Sum types - One of (usually exhaustive) (e.g. data Boolean = True | False)
-- Show example of the expression problem (google it if you don't remember)
---- Tradeoff of adding behaviour vs adding types - think before you design.




-- Exercise 2: Let's implement a Vigenere cypher - https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher

--Example:
--Secret: HATEDOGSHATEDOGSHATEDOGS
--Text:   ILIKECATS
--Output: PLBOHQGLZ (H + I % 26 = P) ...etc

-- Implement a function creating an encoder
-- Member tuples.
extendSecret :: String -> Int -> String
extendSecret s n = if length s < n then extendSecret (s++s) n else s

encodeLetter :: (Char, Char) -> Char
encodeLetter (l, key) = chr $ 65 + mod (ord l + ord key) 26

makeEncoder :: String -> (String -> String)
makeEncoder secret = (\ payload -> map encodeLetter (zip payload $ extendSecret secret $ length payload))
