module Tree
  where

data Tree a = Branch a (Tree a) (Tree a)
            | Leaf a deriving (Show, Eq)

-- | Foldable instance for Tree. Now we can define functions like
--   treeToList below.
instance Foldable Tree where
  foldr = foldTree

-- | Functor instance for Tree. Now we can map over trees, like:
-- > let t1 = Branch 1 (Leaf 2) (Leaf 3)
-- > fmap (2^) t1
-- Branch 2 (Leaf 4) (Leaf 8)
instance Functor Tree where
  fmap = mapTree

-- | Applicative instance for Tree. Now we can do things like:
-- > let t1 = Branch 1 (Leaf 2) (Leaf 3)
-- > let t2 = Branch 10 (Leaf 20) (Leaf 30)
-- > (*) < $ > t1 <*> t2
-- Branch 10 (Leaf 40) (Leaf 90)
instance Applicative Tree where
  pure = Leaf
  (Leaf f) <*> t                      = fmap f t
  (Branch f _ _) <*> (Leaf x)         = Leaf (f x)
  (Branch f l r) <*> (Branch x l' r') = Branch (f x) (l <*> l') (r <*> r')

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

