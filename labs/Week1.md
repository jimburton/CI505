# Exercise Sheet 1

## Starting Haskell and running your first program

Start Linux and open a terminal. Within the terminal, `cd` to the directory in
which you want to store your work. Start the haskell interpreter by typing `ghci`.
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
make changes you need to reload the file in ghci:

```
> :reload Exercises
```

## Exercises

1. Edit `Exercises.hs` to write a function `square` which returns the square
of a number. Test your work in ghci as follows:

```
> :reload Exercises
> square 5
```

What happens when you execute `square True`?

2. Use `square` to write a function `sumsquare :: Int -> Int -> Int` which
returns the sum of the squares of its two arguments.

3. Write a recursive function `memb` which takes two arguments, a list and
an element to search for in that list. Your function should use *pattern
matching* to return `True` if the second input is a member of the list which
is the first input. The base case, which deals with what happens if an
empty list is supplied, should return `False`. The inductive case, which
deals with non-empty lists, should return `True` if the element to be found
matches the head of the list. If not, the function should return the result
of calling itself on the tail of the list. Your function will have this structure: 

```haskell
memb: Eq a => [a] -> a -> Bool
memb [] x     = ...
memb (y:ys) x = ...
```

4. Write a (recursive) function `length’ :: [a] -> Int` which calculates
the length of a list. Think about the base case of the empty list and the
case when the list contains elements.

5. Write a function `drop’ :: Int -> [a] -> [a]`, where `drop’ n xs`
   returns `xs` with its first `n` elements removed.
   
6. Write a function `take’ :: Int -> [a] -> [a]`, where `take’ n xs`
returns the first `n` elements of `xs` as a list (if `xs` contains less than
`n` elements, your function should return all of `xs`).
