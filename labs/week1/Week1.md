# Week 1

## Installing the Haskell compiler and other tools

The core Haskell toolchain consists of the compiler, `ghc`, the
interpreter, `ghci`, and the build tool `cabal`.  These are installed on
Linux in the labs. Install them on your own computer using
[GHCUp](https://www.haskell.org/ghcup/).

`ghc` is used to create stand-alone executable programs. `ghci` is
used to run scripts and to experiment with code. `cabal` is used to
build larger Haskell projects and automates tasks like downloading and
managing dependencies. 

When working on Haskell code you can use any editor to create files
with the extension `.hs` then load them into `ghci` or compile them
with `ghc`. That is probably the best way to go for the first few
weeks of this course. 

Eventually you will want to benefit from syntax highlighting,
auto-completion of function names, easy ways to run the code and all
the other things you expect from an IDE. Haskell plugins are available
for popular IDEs such as VS Code and IntelliJ, and for the power
editors Emacs and Vim. Read more about the options
[here](https://wiki.haskell.org/IDEs).

## Starting Haskell and running your first program

Open a terminal. Within the terminal, `cd` to the directory this file is 
in: `labs/week1`. Start the haskell interpreter by typing
`cabal repl`. This is what that looks like on my system:

```
$ cabal repl
Resolving dependencies...
Build profile: -w ghc-9.0.1 -O1
In order, the following will be built (use -v for more details):
 - week1-0.1.0.0 (exe:week1) (first run)
Configuring executable 'week1' for week1-0.1.0.0..
Preprocessing executable 'week1' for week1-0.1.0.0..
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Week1            ( src/Week1.hs, interpreted )
Ok, one module loaded.
ghci> 
```

Note that this command has loaded the Haskell script `Week1.hs`, which
is currently empty, into `ghci`.  You can type any Haskell expression
into the `ghci` "REPL" (Read-Eval-Print-Loop) and it will be
evaluated.  Enter a list of the integers from 1 to 10 using the range
syntax:

```
λ> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

In the lectures we mentioned the standard functions `map` and `filter`. There is also a standard
function called `odd`, which takes an integer and returns `True` if that number is odd. Try using
`map` and `filter` with the `odd` function and the list from the previous step:

```
λ> map odd [1..10]
[True, False, True, False, True, False, True, False, True, False]
λ> filter odd [1..10]
[1, 3, 5, 7, 9]
```

Use your favourite text editor to edit the file
`labs/week1/src/Week1.hs`, adding this content:

```haskell
test :: String
test = "Hello World!"
```
Going back to the terminal, reload your work and test it like so:
```
> :reload
> test
```

(Note that you can type `:r` as an abbreviation for `:reload`.) Add
your solutions to all of the following problems to
`Week1.hs`. Whenever you make changes you need to reload the file in
the REPL.

## Exercises

1. Edit `Week1.hs` to write a function `square :: Int -> Int` which returns the square
of a number. Test your work in ghci as follows:

   ```
   > :reload 
   > square 5
   ```

   Try calling `square` with a few numbers. What happens when you execute `square True`?

2. Use `square` to write a function `sumsquare :: Int -> Int -> Int` which
returns the sum of the squares of its two arguments.

3. Write a recursive function `elem` which takes two arguments, a
value and a list of values of the same type, and returns a
boolean.

   Your function should use
*pattern matching* to return `True` if the first argument is an *element* of
the list (hence the name). There should be two equations in the definition of the function --
the first will be one which pattern matches the empty list. This should return `False`, since nothing
is an element of the empty list. The second case, which
deals with non-empty lists, should return `True` if the element to be
found is equal to the head of the list. If not, the function should return
the result of calling itself recursively on the tail of the list. Your function
will have the structure below. 

   Note the *typeclass constraint* `Eq a` -- the type of the things in
the list, `a`, must be a type that can be compared for equality:

   ```haskell
   elem :: Eq a => a -> [a] -> Bool
   elem x []     = ...
   elem x (y:ys) = ...
   ```

4. Write a (recursive) function `length :: [a] -> Int` which
calculates the length of a list. Use pattern matching to write an
equation for the empty list (what is its length?) and the case when
the list contains elements. Recursively, the length a list with a head
a tail is one plus the length of the tail.

5. Write a function `drop :: Int -> [a] -> [a]`, where `drop n xs`
   returns `xs` with its first `n` elements removed. Make a function
   with three equations using pattern matching:
   
   ```haskell
   drop :: Int -> [a] -> [a]
   drop 0 xs     = ...
   drop n []     = ...
   drop n (x:xs) = ...
   ```

   So you need to think about how to drop zero elements from a list, how
to drop `n` (any number) of elements from the empty list, and how to drop `n`
elements from a list with a head and a tail (recursively, drop the head then
drop `n-1` elements from the tail.

6. Write a function `take :: Int -> [a] -> [a]`, where `take n xs`
returns the first `n` elements of `xs` as a list (if `xs` contains
less than `n` elements, your function should return all of `xs`). The
definition will be similar to `drop` except that in the recursive
case you have to use "cons", `(:)`, to build up the list that you will return. That
is, to take `n` items from a list with a head and a tail, you will need to cons the head
onto a list which is the result of taking `n-1` elements from the tail.
