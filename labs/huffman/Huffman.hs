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
tree str = undefined 

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
merge hs = undefined

{- 3. Insert a HTree node into a list sorted by ascending frequency.
-}
insert :: HTree a -> [HTree a] -> [HTree a]
insert h hs = undefined

{- 4. Construct the sorted frequency table for the input [a].
-}
fTable :: Ord a => [a] -> FreqTable a
fTable xs = undefined

