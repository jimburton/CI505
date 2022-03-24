-- |
-- Module      : QSort.hs
-- Description : Initial QuickCheck example
-- Maintainer  : j.burton@brighton.ac.uk
-- Stability   : experimental
-- Portability : POSIX
-- 
-- Initial QuickCheck example. Because this isn't part of a cabal project
-- you may need to install several packages manually:
-- 
-- $ cabal install --lib QuickCheck test-framework test-framework-quickcheck2
-- $ ghc --make QSort.hs -o qsort 
-- $ ./qsort

module Main where

import Data.List ((\\), sort)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- * The code under test.

-- | A pseudo-quicksort.
qsort :: Ord a => [a] -> [a] 
qsort [] = [] 
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs 
  where lhs = filter (< x) xs 
        rhs = filter (>= x) xs 

-- * The tests.

-- | Check that the output is sorted.  
prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (qsort xs) 
  where ordered []  = True
        ordered [_] = True 
        ordered (x:y:ys) = x <= y && ordered (y:ys)

-- | Sorting twice makes no difference.  
prop_idempotent :: [Int] -> Bool 
prop_idempotent xs = qsort (qsort xs) == qsort xs

-- | The head is the smallest element. Discards input that doesn't
-- | meet the constraint of being non-empty.  
prop_minimum :: NonEmptyList Int -> Bool 
prop_minimum (NonEmpty xs) = head (qsort xs) == minimum xs

-- | The output contains all and only the elements from the input.
prop_permutation :: [Int] -> Bool 
prop_permutation xs = permutation xs (qsort xs) 
  where permutation xs ys = null (xs \\ ys) && null (ys \\ xs) 

-- | qsort works in the same way as the sort function from Data.List.  
prop_sort_model :: [Int] -> Bool
prop_sort_model xs = sort xs == qsort xs

-- | Collect the tests into a suite.
tests :: [Test]
tests = [ testProperty "Check that the output is ordered." prop_ordered
        , testProperty "Sorting twice makes no difference." prop_idempotent
        , testProperty "The head is the smallest element." prop_minimum
        , testProperty "Input and output contain the same elements." prop_permutation
        , testProperty "Works in the same way as Data.List.sort." prop_sort_model
        ]

-- | The main function runs the tests.
main :: IO ()
main = defaultMain tests
