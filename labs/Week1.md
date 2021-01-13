# Week 1

## Installing the Haskell compiler and other tools

Install the [Haskell
platform](https://www.haskell.org/downloads/#platform), which provides
everything you need to get started. This includes the Haskell
compiler, which is called `ghc` and is used to create stand-alone
executable programs, and the interpeter, which is called `ghci` and is
used to run scripts and to experiment with code. 

As for working on Haskell code, you can use any editor to create files
with the extension `.hs` then load them into `ghci` or compile them
with `ghc`. That is probably the best way to go for the first few
weeks of this course.

But to benefit from syntax highlighting, auto-completion of function
names, easy ways to run the code and all the other things you expect
from an IDE. Haskell plugins are available for popular IDEs such as VS
Code and IntelliJ, and for the power editors Emacs and Vim. Read more
about the options [here](https://wiki.haskell.org/IDEs).

## Starting Haskell and running your first program

Open a terminal. Within the terminal, `cd` to the directory in which
you want to store your work. Start the haskell interpreter by typing
`ghci`. This is what that looks like on my system:

```
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Loaded package environment from /home/jb259/.ghc/x86_64-linux-8.6.5/environments/default
package flags have changed, resetting and loading new packages...
Loaded GHCi configuration from /home/jb259/.ghc/ghci.conf
λ> 
```
You can type any Haskell expression into this "REPL" (Read-Eval-Print-Loop) and it will be evaluated. 
Enter a list of the integers from 1 to 10 using the range syntax:

```
λ> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

In the lectures we mentioned the standard function `map` and `filter`. There is also a standard
function called `odd`, which takes an integer and returns `True` if that number is odd. Try using
`map` and `filter` with the `odd` function and the list from the previous step:

```
λ> map odd [1..10]
[True, False, True, False, True, False, True, False, True, False]
λ> filter odd [1..10]
[1, 3, 5, 7, 9]
```

Use your favourite text editor to create a file called `Exercises.hs` in the
same directory, with this content:

```haskell
module Exercises where
test :: String
test = "Hello World!"
```
Going back to ghci, load your new file and test it like so:
```
> :load Exercises
> test
```

Add your solutions to the following problems to Exercises.hs. When you
make changes you need to reload the file in `ghci`:

```
> :reload Exercises
```

## Exercises

1. Edit `Exercises.hs` to write a function `square :: Int -> Int` which returns the square
of a number. Test your work in ghci as follows:

```
> :reload Exercises
> square 5
```

Try calling `square` with a few numbers. What happens when you execute `square True`?

2. Use `square` to write a function `sumsquare :: Int -> Int -> Int` which
returns the sum of the squares of its two arguments.

3. Write a recursive function `memb` which takes two arguments, a list
and an element to search for in that list. Your function should use
*pattern matching* to return `True` if the second input is a member of
the list which is the first input. The base case, which deals with
what happens if an empty list is supplied, should return `False`
(nothing is a member of the empty list).  The inductive case, which
deals with non-empty lists, should return `True` if the element to be
found matches the head of the list. If not, the function should return
the result of calling itself on the tail of the list. Your function
will have the structure below. 

    Note the *typeclass constraint* `Eq a` -- the type of the things in
the list, `a`, must be a type that can be compared for equality:

```haskell
memb :: Eq a => [a] -> a -> Bool
memb [] x     = ...
memb (y:ys) x = ...
```

4. Write a (recursive) function `length’ :: [a] -> Int` which
calculates the length of a list. Think about the base case (what is
the length of the empty list?) and the case when the list contains
elements (recursively, the length a list with a head a tail is one
plus the length of the tail).

5. Write a function `drop’ :: Int -> [a] -> [a]`, where `drop’ n xs`
   returns `xs` with its first `n` elements removed. Make a function
   with three equations using pattern matching:
   
```
drop' :: Int -> [a] -> [a] -> [a]
drop' 0 xs     = ...
drop' n []     = ...
drop' n (x:xs) = ...
```

So you need to think about how to drop zero elements from a list, how
to drop `n` (any number) of elements from the empty list, and how to drop `n`
elements from a list with a head and a tail (recursively, drop the head then
drop `n-1` elements from the tail.

6. Write a function `take’ :: Int -> [a] -> [a]`, where `take’ n xs`
returns the first `n` elements of `xs` as a list (if `xs` contains
less than `n` elements, your function should return all of `xs`). The
definition will be similar to `drop'` except that in the recursive
case you have to use "cons", `(:)`, to build up the list that you will return. That
is, to take `n` items from a list with a head and a tail, you will need to cons the head
onto a list which is the result of taking `n-1` elements from the tail.
