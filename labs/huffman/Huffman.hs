{- 
   CI505 Introduction to Functional Propgramming
   Huffman Trees Exercise -- Encoding
-}
module Huffman where

import Data.Tree -- Used for pretty-printing trees
import Data.Maybe

{- A FreqTable is a list of pairs where the first element of each pair is
an element and the second element is its frequency 
-}

type FreqTable a = [(a, Int)]

{- A data type of binary trees, HTree a, where each node
carries an int (representing frequency counts) and and leaves carry an
additional piece of data of type a (e.g. a char). A HTree is a Huffman
Tree. In a leaf node, (Leaf i c), i will represent the frequency of
occurence of c. In a branch node (Branch i l r), i will represent the
combined frquencies of the left and right children.

Your data type should derive the `Show` and `Eq` type classes. -}

data HTree a = Leaf Int a | Branch Int (HTree a) (HTree a) deriving (Show, Eq)

{- 1. Construct the Huffman tree for the input [a]. Return Nothing if
the input is empty.  Otherwise, do the following: . construct the
frequency table for str . use the frequency table to create a list of
Leaf nodes . merge that list into a single tree by calling your merge
function
-}


tree :: Ord a => [a] -> Maybe (HTree a)
tree str = if not $ null str 
           then merge $ map (\f -> Leaf (snd f) (fst f)) (fTable str)
           else Nothing

{- 2. Merge a list of HTree nodes into a single Maybe HTree. If the
input is empty, return Nothing. If the input contains a single
element, we are done. Otherwise, do the following: . create a Branch
node from the first two elements in the input, . use your insert
function to insert this new element in the right place in the new
list, which is formed of the old list without its first two elements,
. call merge recursively on the new list

When merging two nodes, n1 and n2, the node with the lowest frequency
will be the left-hand child in the new Branch node.  
-} 
merge :: [HTree a] -> Maybe (HTree a)
merge [] = Nothing
merge (x:[]) = Just x
merge (x:y:zs) = let cX = getFreq x
                     cY = getFreq y
                     i = cX + cY in
                 if cX < cY then
                     merge $ insert (Branch i x y) zs
                 else 
                     merge $ insert (Branch i y x) zs

{- 3. Insert a HTree node into a list sorted by ascending frequency.
-}
insert :: HTree a -> [HTree a] -> [HTree a]
insert h [] = [h]
insert h (t:ts) = if getFreq h < getFreq t then h:t:ts
                  else t : insert h ts

{- 4. Construct the sorted frequency table for the input [a].
-}
fTable :: Ord a => [a] -> FreqTable a
fTable = sort' . freq 
    where freq [] = []
          freq (c:cs) = freqInner (c,1) cs : freq (filter (/=c) cs)
              where freqInner (d,n) [] = (d,n)
                    freqInner (d,n) (e:es) = let m = if d==e then n+1 else n
                                             in freqInner (d,m) es

-- A getter for the frequency count in a HTree node.
getFreq :: HTree a -> Int
getFreq (Branch i _ _) = i
getFreq (Leaf i _) = i

-- Pseudo-QuickSort 
sort' :: Ord b => [(a,b)] -> [(a,b)]
sort' [] = []
sort' (x:xs) = (sort' lt) ++ [x] ++ (sort' gt)
    where lt = filter (\(y,z) -> z < snd x) xs
          gt = filter (\(y,z) -> z >= snd x) xs
