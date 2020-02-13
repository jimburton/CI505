module Main where

import Test.QuickCheck

import Huffman

-----------------------------------
-- TESTS
-----------------------------------

isSorted :: FreqTable a -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = if snd x <= (snd $ head xs) 
                  then isSorted xs else False

prop_fTable_sorted :: String -> Bool
prop_fTable_sorted xs = isSorted $ fTable xs 

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

main = do quickCheck prop_fTable_sorted
          quickCheck prop_fTable_els
          quickCheck prop_fTable_els2
          quickCheck prop_countNodes
