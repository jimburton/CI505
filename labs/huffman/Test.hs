{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import System.Random
import Control.Monad
import Data.List.Utils (countElem)
import Data.List       (nub)
import Data.Maybe      (fromJust)


import Huffman

instance Arbitrary a => Arbitrary (HTree a) where
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (HTree a)
arbitrarySizedTree 0 = do c <- arbitrary
                          i <- arbitrary
                          return (Leaf i c)
arbitrarySizedTree n | n>0 = do
                         c <- arbitrary
                         i <- arbitrary
                         oneof [return (Leaf i c),
                                liftM3 Branch arbitrary subtree subtree]
  where subtree = arbitrarySizedTree (n `div` 2)

-----------------------------------
-- TESTS
-----------------------------------

isSorted :: Ord b => (a -> b) -> [a] -> Bool
isSorted _ []     = True
isSorted _ [x]    = True
isSorted f (x:xs) = if f x <= (f $ head xs) 
                    then isSorted f xs else False

{- Frequence tables are properly sorted -}
prop_fTable_sorted :: String -> Bool
prop_fTable_sorted = isSorted snd . fTable

{- Frequency tables really reflect the input -}
prop_fTable_valid :: String -> Bool
prop_fTable_valid s = let t = fTable s in
  length t == length (nub s)
  && (and $ map (\c -> countElem c s == fromJust (lookup c t)) (nub s) )


prop_fTable_els :: String -> Bool
prop_fTable_els xs = and $ map (\(c, i) -> c `elem` xs) $ fTable xs 

prop_fTable_els2 :: String -> Bool
prop_fTable_els2 xs = let cs = map fst $ fTable xs in
                      and $ map (\c -> c `elem` cs) xs 
countLeaves t = case t of
                 (Branch _ l r) -> (countLeaves l) + (countLeaves r)
                 (Leaf _ _)     -> 1

prop_countNodes :: String -> Bool
prop_countNodes str = (length $ fTable str) == maybe 0 countLeaves (tree str)

      
prop_insert :: Eq a => HTree a -> SortedList (HTree a) -> Bool
prop_insert t (Sorted ts) = let ts' = insert t ts in
                              length ts + 1 == length ts'
                              && isSorted id ts'

{-
prop_merge :: Eq a => [HTree a] -> Bool
prop_merge xs | null xs        = (merge xs) == Nothing
              | length xs == 1 = (merge xs) == Just (head xs)
              | otherwise      = let t = fromJust (merge xs) in
                                   leftSorted t
  where leftSorted (Leaf _ _)     = True
        leftSorted (Branch _ l r) = getFreq l <= getFreq r
                                    && leftSorted l
                                    && leftSorted r
-}

prop_codec :: Ord a => [a] -> Bool
prop_codec s = case encode s of
  Nothing        -> null s
  Just (t, path) -> s == decode t path


{-
TODO Test these properties
-}
prop_tree :: String -> Bool
prop_tree s = case tree s of
  Nothing  -> null s
  (Just t) -> True -- TODO

prop_generateCode :: HTree a -> Bool
prop_generateCode t = True -- TODO

prop_encode :: Ord a => [a] -> Bool
prop_encode xs = True

prop_decode :: HTree a -> Bool
prop_decode t = True


return []
runTests = $quickCheckAll

main = runTests
