{- 
   CI505 Introduction to Functional Propgramming
   Huffman Trees Exercise -- Encoding
-}
module Huffman where

import Data.Tree -- Used for pretty-printing trees
import Data.Tree.Pretty 
import Data.Maybe

{- A FreqTable is a list of pairs where the first element of each pair is
an element and the second element is its frequency 
-}

type FreqTable a = [(a, Int)]

{-
A data type of binary trees, HTree a, where each node
carries an int (representing frequency counts) and and leaves carry an
additional piece of data of type a (e.g. a char). A HTree is a Huffman
Tree. In a leaf node, (Leaf i c), i will represent the frequency of
occurence of c. In a branch node (Branch i l r), i will represent the
combined frquencies of the left and right children.
 -}

data HTree a = Leaf Int a | Branch Int (HTree a) (HTree a) deriving (Show, Eq)

getFreq :: HTree a -> Int
getFreq (Branch i _ _) = i
getFreq (Leaf i _)     = i

{-
0. Make HTree into an instance of Ord -- t1 is less than t2 if the Int
label of t1 is smaller than that of t2 (it doesn't matter whether
either is a Leaf or a Branch).
-}

instance Eq a => Ord (HTree a) where
  (<=) (Leaf i _) (Leaf j _)         = i<=j
  (<=) (Leaf i _) (Branch j _ _)     = i<=j
  (<=) (Branch i _ _) (Leaf j _)     = i<=j
  (<=) (Branch i _ _) (Branch j _ _) = i<=j
  
{-
Types to represent the Huffman encoded data.
-}
data PathPart a = L | R | E a            deriving Show
type Path a     = [PathPart a]
type HCode a    = [(a, Path a)]
type HEncoded a = (HTree a, Path a)

{-
1. Complete the `ftable` function which constructs the sorted
   frequency table for the input `[a]`.
-}

fTable :: Ord a => [a] -> FreqTable a
fTable = sort' . freq 
    where freq [] = []
          freq (c:cs) = freqInner (c,1) cs : freq (filter (/=c) cs)
              where freqInner (d,n) [] = (d,n)
                    freqInner (d,n) (e:es) = let m = if d==e then n+1 else n
                                             in freqInner (d,m) es

-- Pseudo-QuickSort 
sort' :: Ord b => [(a,b)] -> [(a,b)]
sort' [] = []
sort' (x:xs) = sort' lt ++ [x] ++ sort' gt
    where lt = filter (\(y,z) -> z < snd x) xs
          gt = filter (\(y,z) -> z >= snd x) xs


{-
2. Complete the `insert` function, which inserts a `HTree` node into a
   list sorted by ascending frequency.
-}

insert :: HTree a -> [HTree a] -> [HTree a]
insert h [] = [h]
insert h (t:ts) = if getFreq h < getFreq t then h:t:ts
                  else t : insert h ts

{-
3. Merge a list of `HTree` nodes into a single `Maybe HTree`. If the input
is empty, return `Nothing`. If the input contains a single element, we
are done. Otherwise, do the following: 
    * create a `Branch` node from the first two elements in the input,
	* use your `insert` function to insert this new
      element in the right place in the new list, which is formed of the old 
      list without its first two elements,
    * call `merge` recursively on the new list.


When merging two nodes, `n1` and `n2`, the node with the lowest frequency
will be the left-hand child in the new `Branch` node.
-}

merge :: [HTree a] -> Maybe (HTree a)
merge []       = Nothing
merge [x]      = Just x
merge (x:y:zs) = let cX = getFreq x
                     cY = getFreq y
                     i = cX + cY in
                 if cX < cY then
                     merge $ insert (Branch i x y) zs
                 else 
                     merge $ insert (Branch i y x) zs

{-
4. Complete the `tree` function, which constructs the Huffman tree for
the input `[a]`. Return `Nothing` if the input is empty.  Otherwise, do
the following: 
    * construct the frequency table for `str`,
	* use the frequency table to create a list of Leaf nodes, 
	* merge that list into a single tree by calling your `merge`
      function.

-}

tree :: Ord a => [a] -> Maybe (HTree a)
tree str = if not $ null str 
           then merge $ map (\f -> Leaf (snd f) (fst f)) (fTable str)
           else Nothing

{-
5. Complete the `generateCode` function, which retrieves the code
   embodied by a Huffman tree. The function returns a `HCode`, which
   is a lookup table of `Code` values, each of which is a `Path` from
   the root to one of the leaves of the tree, indexed by the value
   that is found at the leaf.
-}

generateCode :: Ord a => HTree a -> HCode a
generateCode (Leaf i c)     = []
generateCode (Branch i l r) = map toLookup $ paths L l ++ paths R r
  where paths dir (Leaf _ c)     = [[dir, E c]]
        paths dir (Branch _ l r) = map (dir :) (paths L l) ++ map (dir :) (paths R r)
        toLookup ps                   = let (E c) = last ps in (c, init ps)

{-
6. Complete the `encode` function, which creates the tree for the
   input, `str`, generates the code for that tree, and uses it to
   encode `str`. It returns a pair of the encoded input and the HCode
used to encode it.
-}

encode :: Ord a => [a] -> Maybe (HEncoded a)
encode str = tree str >>= \t -> let code = generateCode t in
  return (t, concat $ concat <$> sequenceA $ map (`lookup` code) str)


{-
7. Complete the `decode` function, which takes some encoded input and
   a `HCode` object and returns the decoded result.
-}

decode :: HTree a -> Path a -> [a]
decode t pps = decodeInner t pps
  where decodeInner (Leaf _ c) []          = [c]
        decodeInner (Leaf _ c) input       = c : decodeInner t input
        decodeInner (Branch _ l _) (L:ps)  = decodeInner l ps 
        decodeInner (Branch _ _ r) (R:ps)  = decodeInner r ps
        decodeInner _ []                   = []

------------------
-- Drawing trees
------------------

toDataTree :: Show a => HTree a -> Data.Tree.Tree String
toDataTree (Leaf i c) = Node (showLeafData c i) []
toDataTree (Branch i l r) = Node (show i) [toDataTree l, toDataTree r]

showLeafData :: (Show a, Show b) => a -> b -> String
showLeafData x y = show y ++ ":" ++ show x

printMaybeTree :: Show a => Maybe (HTree a) -> IO ()
printMaybeTree Nothing  = putStrLn ""
printMaybeTree (Just t) = putStrLn $ drawVerticalTree $ toDataTree t
