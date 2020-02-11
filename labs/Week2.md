# Exercise Sheet 2

Add your solutions to the following problems to the file you created last time. When you make changes you need to reload the file in ghci:

```
> :reload Exercises
```

**Hint:** use the hints given for each exercise.

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

**Hint:** `map` a lambda function over the list of lists that you get
back from `pack`. This function will take a list as input and return a
pair of its length and its first element.

3. Given a run-length code list generated as in the previous problem, construct its uncompressed version.

Example:
```haskell
λ> decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
"aaaabccaadeeee"
```

**Hint:** Again, a good way to start is to `map` a lambda over the
input. This function will take a pair, `(i,c)`. Use the functions
`repeat` and `take` to create a list of `c` values with length
`i`. This will result in a **list of lists** which you can flatten with
the `concat` function. 

So `decode` could have this kind of structure:

```haskell
decode xs = concat (map (...) xs)
```

Using `map` then `concat` is very common -- **there is a function that
does them both for you**:
[`concatMap`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:concatMap). Use
that instead of the structure suggested above.

4. Create a data type, `RLE a` (for "run-length encodings" of values
   of type `a`). Use the `deriving` keyword to make it a member of the
   `Show` typeclass. It needs two constructors: `Single a`, to
   represent a single occurence of a character in a list, and
   `Multiple Int a` to represent several consecutive occurrences of a
   value. 
   
   Create a modified version of the `encode` function that
   produces a list of `RLE a` values instead of pairs.

	Example:

```haskell
λ> encodeRLE "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
```

**Hint:** First, process the input with `encode`, then `map` over the
result. The lambda that you map will take a pair, `(i,c)`, and use one
of the `RLE` constructors if `i==1`, and a different one otherwise.

5. Given a run-length code list generated as in the previous problem, construct its uncompressed version.

Example:
```haskell
λ> decodeRLE [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
```

**Hint:** Again, you can use `concatMap`. This time the lambda that
you map over the input will take an `RLE a` value. Use the `case`
structure in the lambda to pattern match on the different values of
`RLE`.
