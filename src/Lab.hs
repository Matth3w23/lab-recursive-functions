--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive functions                                                   --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, subsequences )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) = (a == x) || elem a xs

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a [x] = [x]
intersperse a x = head x : a : intersperse a (tail x)

subsequences :: [a] -> [[a]] --incorrect, proper logic see solutions branch
subsequences [] = [[]]
subsequences x = [] : innerSubsequences x where
    innerSubsequences [x] = [[x]]
    innerSubsequences x = firstSubSequences x ++ innerSubsequences (tail x) where
        firstSubSequences [x] = [[x]]
        firstSubSequences x = x : firstSubSequences (init x)

--------------------------------------------------------------------------------
