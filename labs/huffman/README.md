# Huffman Trees

Enter your solutions into the file [Huffman.hs](Huffman.hs). Use
`cabal` to test and run the code.  

## Encoding

After reading [the slides](../huffman-slides.pdf) about Huffman trees
you should have an understanding of the data structures and algorithms
involved. We will be modelling *frequence tables* as follows:

```haskell
type FreqTable a = [(a, Int)]
```

The module also contains a data type of binary trees, `HTree a`, where
each node carries an int (representing frequency counts), and
leaves carry an additional piece of data of type a (e.g. a char). A
`HTree` is a Huffman Tree. In a leaf node, (`Leaf i c`), `i` represents
the frequency of occurence of `c`. In a branch node (`Branch i l r`), `i`
represents the combined frquencies of the left and right children.

```haskell
data HTree a = Leaf Int a | Branch Int (HTree a) (HTree a) deriving (Show, Eq)
```

1. Complete the `ftable` function which constructs the sorted
   frequency table for the input `[a]`.

2. Complete the `insert` function, which inserts a `HTree` node into a
   list sorted by ascending frequency.

3. Merge a list of `HTree` nodes into a single `Maybe HTree`. If the input
is empty, return `Nothing`. If the input contains a single element, we
are done. Otherwise, do the following: 
    * create a `Branch` node from the first two elements in the input,
	* use your `insert` function (see below) to insert this new
      element in the right place in the new list, which is formed of the old 
      list without its first two elements,
    * call `merge` recursively on the new list.

When merging two nodes, `n1` and `n2`, the node with the lowest frequency
will be the left-hand child in the new `Branch` node.

4. Complete the `tree` function, which constructs the Huffman tree for
the input `[a]`. Return `Nothing` if the input is empty.  Otherwise, do
the following: 
    * construct the frequency table for `str`,
	* use the frequency table to create a list of Leaf nodes, 
	* merge that list into a single tree by calling your `merge`
      function (which is the next problem).

5. Complete the `generateCode` function, which retrieves the code
   embodied by a Huffman tree. The function returns a `HCode`, which
   is a lookup table of `Code` values, each of which is a `Path` from
   the root to one of the leaves of the tree, indexed by the value
   that is found at the leaf.
   
6. Complete the `encode` function, which creates the tree for the
   input, `str`, generates the code for that tree, and uses it to
   encode `str`.

7. Complete the `decode` function, which takes some encoded input and
   a `HCode` object and returns the decoded result.





