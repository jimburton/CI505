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

As an extension, add the `Nat` datatype to the `Integral` typeclass. These are the types for which
the following functions are defined:

```
toInteger :: a -> Integer
quotRem   :: a -> (a,a) 
```

The `toInteger` function should be obvious...`quotRem` needs to take two `Nat`s, say `n` and `m`, and
return a pair containing the `Nat`s which are the number of times `m` goes into `n` and the remainder 
after that division.

Before you can declare the `Integral` instance of `Nat`, you need to declare it as an `Ord`, `Enum`
and `Real`. The `Ord` typeclass is for those types that can be ordered (i.e. values can be less than 
or greater than each other). The typeclass instance needs to define the `compare` method which should
take two `Nat`s, say `n` and `m`, and return an `Ordering`, which is `LT` if `n` is less than `m`, `EQ`
if they are equal, or `GT` if `n` is greater than `m`.

To declare `Nat` as an `Enum` you need to define the functions `toEnum` and `fromEnum`. The `toEnum`
function takes an `Integer`, `n`, and returns a `Nat`. If `n` is less than zero throw an `error`, 
if it is zero return `Z`, and so on. The `fromEnum` function takes a `Nat` and returns an `Integer`.

The `Real` typeclass requires a single function, `toRational`, which
takes a `Nat` and returns a `Rational`. You can use the `fromEnum`
function to get an `Integer` based on your `Nat`, then use the built-in function `fromIntegral` to
convert this to a `Rational`.

Finally, you can create the `Integral` instance for `Nat`. The
`toInteger` function is easy, and will be identical to
`toRational`. 

The `quotRem` function is a bit trickier. If we call `quotRem n m` we
need to know how many times `m` goes into `n`, and what the remainder
is after the division. If `m` is bigger than `n` it goes into it zero
times with `n` left over so the answer is `(Z, n)`. Otherwise, we can
count the number of times `m` goes into `n` by recursively subtracting
`m` from `n` and adding one to the first element of the pair that
forms the result each time. So in this case you will make a recursive call to `quotRem`
passing in `(n-m)` as the first argument and `m` (unchanged) as the second. If the
result of this recursive call is the pair `(n', m')` then the result of the whole
function is the pair of the *successor* of `n'` (`S n'`, this is the bit that is "counting" how
many time the function has been called, i.e. how many times `m` goes into `n`)
and the remainder `m'`.
