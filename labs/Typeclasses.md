# Week 2 -- datatypes and typeclasses

Complete the exercises below in a new Haskell script, `Exercises2.hs`,
which begins with the following declaration:

```Haskell
  module Exercises2 where
```

In the lecture on algebraic data types, the type of natural numbers
based on a successor function was introduced. In this system, every
number is either zero (`Z`) or the successor of another
number. Thus, the number one, `(S Z)`, is the successor of
(is one greater than) zero. This number system is known as
*Peano* (pronounced *pee-ah-no*) numbers.

So we have an ADT called `Nat` with two constructors, one of which is recursive:

```Haskell
data Nat = Z | S Nat deriving (Show, Eq)
```

Definitions for adding and subtracting `Nat`s were given in the lecture notes. 

We also discussed *typeclasses* such as `Show` (the class of all ADTs
that can be converted to strings), `Eq` (those ADTs that can be
comparted for equality) and `Num` (the numeric types). If we make
`Nat` an instance of `Num`, we can use `Nat` values wherever a
numeric type is expected. In order to do this we have to provide
definitions for the following functions:

+ Addition and subtraction (given in the lecture notes).
+ Multiplication `(*)`: we can define this using addition. To multiply two `Nat`s,
    `n` and `m`, add `m` to itself `n` times.
+ `negate`, which negates a `Nat`. The negation of `Z` is `Z`. There
    are no negative Peano numbers, so negating anything other than `Z`
    should return an error.
+ `signum`, which returns the sign of a number. For "ordinary"
    numbers, i.e. ones which may be positive or negative, `signum`
    returns -1 for negative numbers, 0 for zero and 1 for positive
    numbers. Yours should do the same, except that you only need to
    consider zero and positive numbers.
+ `abs`, which returns the absolute value of a number. Since there are no
    negative Peano numbers, you should just return the number itself.
+ `fromInteger`, which converts an `Integer` value to a `Nat`.

To make `Nat` an instance of `Num`, copy its definition and those of
`add` and `sub` into your script then complete the following code snippet:

```Haskell
instance Num Nat where
  n + m = ...
  n - m = ...
  n * m = ...
  negate n = ...
  signum n = ...
  abs n = ...
  fromInteger i = ...
```

Note that you can use pattern matching in this code, so you may end up with several definitions
for some functions. For example, the most convenient way to define `fromInteger` may be

```Haskell
instance Num Nat where
  ...
  fromInteger 0 = ...
  fromInteger n = ...
```

Once you have made `Nat` an instance of `Num`, experiment with using the standard
numeric operations on its values in `ghci`:

```Haskell
> (S (S Z)) + (S (S Z))
(S (S (S (S Z))))
> (S (S Z)) * (S Z)
(S (S Z))
-- and so on...
```
