# Datatypes and typeclasses

Complete the exercises below in a new Haskell script, `Peano.hs`,
which begins with the following declaration:

```Haskell
  module Peano where
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

# Make a `Nat` instance of `Num`

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

# Make a `Nat` instance of `Integral`

As an extension, add the `Nat` datatype to the `Integral` typeclass,
which provides whole-number division and remainder operations. 

There is a hierarchy of typeclasses at work here -- to make a type
into an instance of `Integral`, it must already be an instance of
`Enum`, `Ord` and `Real`.

## Make a `Nat` instance of `Ord`

The `Ord` typeclass is for those types that can be ordered
(i.e. values can be less than or greater than each other). The
typeclass includes the `(<)`, `(>)`, `(<=)` and `compare`
functions. Actually, all you need to do is define `compare`, then the
compiler can infer the others for you. A definition of `compare` is
the "minimal definition" for this typeclass.  The `compare` function
should take two `Nat`s, say `n` and `m`, and return an `Ordering`,
which is the value `LT` if `n` is less than `m`, `EQ` if they are equal, or `GT`
if `n` is greater than `m`. Your code will look like something this:

```
instance Ord Nat where 
  compare n m = ...

```

but using pattern matching on `n` and `m` may be the neatest way to write it.

## Make a `Nat` instance of `Enum`

To declare `Nat` as an `Enum` you need to define the functions `toEnum` and `fromEnum` (there
are other functions in the typeclass, but this is the minimal definition). The `toEnum`
function takes an `Integer`, `n`, and returns a `Nat`. If `n` is less than zero throw an `error`, 
if it is zero return `Z`, and so on. The `fromEnum` function takes a `Nat` and returns an `Integer`.

```
instance Enum Nat where
  toEnum n = ...
  fromEnum n = 0

```
## Make a `Nat` instance of `Real`

`Real` is rather confusingly named -- rather than being the 
typeclass of real numbers (those with a decimal fraction) it is the type class of numbers
that aren't irrational (like `e` or `pi`).

The typeclass requires a single function, `toRational`, which
takes a `Nat` and returns a `Rational`. You can use the `fromEnum`
function to get an `Integer` based on your `Nat`, then use the built-in function `fromIntegral` to
convert this to a `Rational`.

```
instance Real Nat where
  toRational n = ...
```
## The `Nat instance of `Integral`

Finally, you can create the `Integral` instance for `Nat`. This
requires you to define the following functions:

```
instance Integral Nat where
  toInteger n = ...
  quotRem n d = ...

```

The `toInteger` function is easy, and will be identical to
`toRational`. 

The `quotRem` function is a bit trickier. The type of `quotRem` is

```
quotRem :: Integral a => a -> a -> (a, a)
```
Calling `quotRem n m` divides `n` by `m` then returns the pair of the result
of the division and the remainder. There are two cases:

1. If `m` is bigger than `n` it goes into it zero
times with `n` left over so the answer is `(Z, n)`. 
2. Otherwise, we can count the number of times `m` goes into `n` by
recursively subtracting `m` from `n` and adding one to the first
element of the pair that forms the result each time. So in this case
you will make a recursive call to `quotRem` passing in `(n-m)` as the
first argument and `m` (unchanged) as the second. If the result of
this recursive call is the pair `(n', m')` then the result of the
whole function is the pair of the *successor* of `n'` (`S n'` -- this is
the way we are "counting" how many time the function has been called,
i.e. how many times `m` goes into `n`) and the remainder `m'`. Eventually, `n`
will be less than `m` and the recursion will end.


