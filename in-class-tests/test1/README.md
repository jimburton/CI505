# In-Class Test 1 -- List functions

This test should be completed as a `cabal` project. You will make
your changes to the module [`Main.hs`](Main.hs) in the same
directory as this file but rather than downloading just that file you
need a copy of the entire directory (as it has the cabal configuration file 
and other necessary items). The easiest way to do that is by using `git`:

```
$ git clone https://github.com/jimburton/CI505
cloning into CI505 ...
$ cd CI505/in-class-tests/test1/
```

As the same time as having your editor open,
you will want to run the code in a terminal to test your changes.  Use
the `cabal repl` command to do this:

```
$ cabal repl
Build profile: -w ghc-8.6.5 -O1
...
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.

```

The REPL does tab completion so you can type the beginning of a
function name and press tab to complete it. Try entering `my` and
pressing tab.

In addition to playing with the functions yourself, each of them has
at least one automated test. These are collected at the end of
`Main.hs` -- have a look at what's there. Run tests in the REPL by
entering `quickCheck <name-of-test.>` or run all of them at once by
entering `main`. 

```
λ> quickCheck prop_dropK
+++ OK, passed 100 tests.
λ> main
+++ OK, passed 100 tests; 110 discarded.
+++ OK, passed 100 tests; 218 discarded.
+++ OK, passed 100 tests; 46 discarded.
+++ OK, passed 100 tests; 50 discarded.
+++ OK, passed 100 tests; 244 discarded.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests; 102 discarded.
+++ OK, passed 100 tests; 323 discarded.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests; 11 discarded.
+++ OK, passed 100 tests; 826 discarded.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

```

When you have completed the test, submit the file `Main.hs` on My
Studies to get feedback. You can also take a look at the answers in
the version of `Main.hs` in the `solutions` branch, but make sure to
discuss your work with your tutor.

1. Complete the function
   ```haskell
   myTakeWhile :: (a -> Bool) -> [a] -> [a]
   myTakeWhile p [] = undefined
   myTakeWhile p (x:xs) = undefined
   ```

   where `myTakeWhile p xs` returns elements of `xs` as a list until it
   reaches an element of `xs` for which `p` is false. For example,
   `myTakeWhile (\x -> x < 3) [1, 2, 3, 4]` returns `[1, 2]`.

2. Complete the `penultimate` function. It should find the penultimate
   (second to last) element in a list. Behaviour is undefined if the input
   is a list with fewer than 2 elements (i.e. you don't have to worry about it!).

3. Complete the `findK` function. Find the element at index `k` in list `l`. For example: `findK 2
[0,0,1,0,0,0]` returns `1`. Behaviour is undefined if there is no `k`th element in the list.

14. Complete the `palindrome` function. It should determine if a list, `l`, is a palindrome.

5. Duplicate the elements in list `xs`. For example `duplicate
[1,2,3]` should give the list `[1,1,2,2,3,3]`. Hint: The `concat`
function flattens a list of lists into a single list. For example:
`concat [[1,2,3],[3,4,5]]` returns `[1,2,3,3,4,5]`.

6. Complete the `splitAtIndex` function. Split a list, `l`, at element `k` into a tuple containing the first
part of `l` up to and including `k`, followed by the second part of
`l` after `k`. For example `splitAtIndex 3 [1,1,1,2,2,2]` returns
`([1,1,1],[2,2,2])`. Behaviour is undefined if there is no `k`th element in the list.

7. Complete the `dropK` function. Drop the element at index `k` in
list `l`. For example `dropK 3 [0,0,0,1,0,0,0]` returns
`[0,0,0,0,0,0]`. If there is no `k`th element in the list `dropK`
returns its input unchanged.

8. Complete the `slice` function. Extract elements between `i`th and `k`th element in list `l`,
including `i`, but not `k`. For example, `slice 3 6
[0,0,0,1,2,3,0,0,0]` returns `[1,2,3]`. If `k` is greater than the length of the list `slice` should
return all elements from `i` to the end of the list. If `i` is greater than the length of the list `slice`
should return the empty list. Behaviour is undefined if `i` is not less than `k`.

9. Complete the `insertElem` function. Insert element `x` in list `l` at index `k`. For example,
`insertElem 2 5 [0,0,0,0,0,0]` returns `[0,0,0,0,0,2,0]`. If `k` is greater than or equal to the length 
of the list `insertElem` adds `x` as the last element in the list.

10. Complete the `rotate` function. Rotate list `l` `n` places left. For example, `rotate 2
[1,2,3,4,5]` gives `[3,4,5,1,2]`.
