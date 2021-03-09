module Tree
  where

import Data.Foldable

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap = mapTree

instance Foldable Tree where
  foldr = foldTree

-- | Fold a binary function over a Tree.
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f x (Leaf y)       = f y x
foldTree f x (Branch y l r) = let z = foldTree f x l in
                                f y (foldTree f z r)

-- | Map a unary function over a Tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)       = Leaf (f x)
mapTree f (Branch x l r) = Branch (f x) (mapTree f l) (mapTree f r)

-- | Convert a Tree to a list.
treeToList :: Tree a -> [a]
treeToList = foldTree (:) []

-- | Add the labs in a Tree labelled by numbers.
sumTree :: Num a => Tree a -> a
sumTree = foldTree (+) 0

-- | Find the largest positive number in a Tree labelled by Ints.
maxTree :: (Num a, Ord a) => Tree a -> a
maxTree = foldTree max 0

