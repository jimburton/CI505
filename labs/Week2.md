# Exercise Sheet 2

Add your solutions to the following problems to the file you created last time. When you make changes you need to reload the file in ghci:

```
> :reload Exercises
```

## Exercises

1. Pack consecutive duplicates of list elements into sublists. If a
   list contains repeated elements they should be placed in separate
   sublists. We want the function to work for any data type that can
   be compared for equality, i.e. is a member of the `Eq` type
   class. So your function should have this type signature:
	
```
pack :: Eq a => [a] -> [[a]]
```

Example:

```haskell
λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
``` 

**Hint** One way to achieve this is by using the functions [takeWhile](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:takeWhile) and [dropWhile](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:dropWhile).
	
	
2. Run-length encoding of a list. Use your `pack` function to
   implement the so-called *run-length encoding* data compression
   method. Consecutive duplicates of elements are encoded as pairs `(n, c)`, where `n` is the number of duplicates of the element `c`.

Example:
```haskell
λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
```

3. Given a run-length code list generated as in the previous problem, construct its uncompressed version.

Example:
```haskell
λ> decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
"aaaabccaadeeee"
```

4. Create a data type, `RLE a` (for "run-length encodings" of values
   of type `a`, where `a` must be a member of the `Eq` typeclass). It
   needs two constructors: `Single a`, to represent a single occurence
   of a character in a list, and `Multiple Int a` to represent several
   consecutive occurrences of a value. Create a modified version of
   the `encode` function that produces a list of `RLE a` values
   instead of pairs.

	Example:

```haskell
λ> encodeRLE "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
```

5. Given a run-length code list generated as in the previous problem, construct its uncompressed version.

Example:
```haskell
λ> decodeRLE [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
```
