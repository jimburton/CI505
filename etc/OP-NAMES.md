# How to pronounce/name Haskell operators

Unlike many languages, Haskell allows us to define our own *binary
operators*, i.e. functions that take two arguments and that are defined 
in an *infix* way. That is, the first argument comes before the operator,
the second one comes after it. Define an operator just like any other function
but put its name in brackets:

```
-- Logical Exclusive Or, pronounced "xor"
(<+>) :: Bool -> Bool -> Bool
(<+>) True p  = not p
(<+>) False p = p

> True <+> True -- pronounce it "true xor true"
False
> False <+> True
True
> (<+>) False True
True

```

Haskell code typically includes lots of operators, both in the
standard library and in other people's code. This is especially true
with code that uses functors, applicative functors, monads and so
on. Since we often need to read our code out loud to others (and
ourselves!), these things have developed names. 

In addition to that, Haskell code is full of other characters like `::` and `->` that you 
may wonder how to pronounce. Here are some of the
most common nicknames:

| Operator | Name             | Example                                   |
| -------- | ---------------- | ----------------------------------------- |
| `::`     | has type / as    | `f x :: Int`: `f x` *has type* `Int`      |
| `:`      | cons             | `'c':"at"`: `c` *cons* `at`             |
| `$`      | dollar / apply   | usually no need to pronounce this         |
| `.`      | compose / dot / after  | `f . g`: `f` *after* `g`                  |
| `[]`     | the empty list   |                                           |
| `++`     | append           | `xs ++ ys`: `xs` *append* `ys`            |
| `->`     | to               | `a -> b`: `a` *to* `b`                    |
| `\`      | lambda           |                                           |
| `@`      | as               | `foo ll@(l:ls)`: `foo ll` *as* `l cons ls` |
| `>>=`    | bind             |                                           | 
| `<-`     | bind             | (as it desugars to `>>=`)                 |
| `>>`     | then             |                                           |
| `*>`     | then             |                                           |
| `<$>`    | (f)map           |                                           |
| `<$`     | map-replace by   | `0 <$ f`: `f` *map-replace by* `0`        |
| `<*>`    | ap(ply)          | (as it is the same as `Control.Monad.ap`) |
| `!!`     | index            |                                           |
| `!`      | index / strict   | `foo !x`: `foo` *strict* `x`              |  
| `<\|>`    | or / alternative | `expr <\|> term`: `expr` *or* `term`       |
| `~`      | lazy             | `foo ~(a,b)`: `foo` *lazy pair* `a, b`    |


https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
