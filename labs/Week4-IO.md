# Monad exercises

## Randomness and De-sugaring `do`

Pseudo-random number generators in Haskell are similar to those found
in other languages. You create a *generator*, optionally
supplying a *seed* value which tells the generator where to
start, and you can keep asking the generator for the next value.

Use `getStdGen` or `newStdGen` to get hold of a random generator, or
`mkStdGen` to create one based on a seed. Getting a generator is an IO action, for pretty
obvious reasons, but many of the functions that make use of generators are pure -- given the
same generator they will always produce the same result. For instance, `randomR (0.0, 1.0)
  gen` returns a tuple of a number between 0 and 1 and the new generator:

```Haskell
> gen <- newStdGen
> fst $ randomR (0.0, 1.0) gen
3.693355234815632e-2 
```

Haskell can figure out what kind of result we want (an `Int`, a `Float`, a
`Char`, etc) from the arguments supplied. Full details are available at
https://hackage.haskell.org/package/random-1.0.0.3/docs/System-Random.html.

1. Referring to the lecture notes from this week, transform the
   following code to use monadic style instead of do-notation:

```Haskell
import System.Random

randomChars :: RandomGen g => g -> String
randomChars gen = randomRs ('a', 'z') gen

main = do
  g <- newStdGen
  let cs = randomChars g
      tenChars = take 10 cs
  putStrLn tenChars
```

2. Given the `Coin` datatype below, generate an infinite sequence of coin tosses:

```Haskell
data Coin = Head | Tail
```
