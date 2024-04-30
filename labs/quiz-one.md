# Quiz One

1. Use a left fold to define the functions `and` and `or`, which
   behave in the same way as their namesakes from the Prelude.
   
2. Use a fold (right or left) to define `xorList`, which takes a list
   of Booleans and returns `True` if one but not all of them is
   `True`. Note that this definition will require more than a single
   call to the fold function. **Bonus points** if you can avoid
   traversing the list twice.
   
3. Why would you use a left fold rather than a right one, or vice
   versa? What are the *strict* folds and when would you use them? 
   
4. Use pattern matching to write a `Functor` instance for the
   following data type:

   ```haskell
   data Tree a = Leaf a | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)
   ```
5. What are the *functor laws*? Write tests which show that the laws
   hold for `Tree`.

6. Write a `Foldable` instance for `Tree`.

See (Week2.md)[Week2.md] and (Typeclasses.md)[Typeclasses.md]

## Hard

7. This is the `Either` type:

   ```haskell
   data Either a b = Left a | Right b
   ```
   
   It is often used to model computations that might go wrong, where
   `Left` values carry an error message and `Right` values carry a
   successful result. To make a `Functor` instance for `Either` that
   will be used in this way would define the instance for something
   like `Either Text`, where `Text is the type of error
   messages. Explain how this differs from the `Functor` instance for
   `Tree` and why the type of `Functor` typeclass requires it.
   
8. Define a `newtype`,  `Try a`, for `(Either Text a)`, and a `Functor
` instance for `Try`.

9. Define an `Applicative` instance for `Try`.

10. Define a `Monad` instance for `Try`.

11. Rewrite the code below to use `Try` and applicative and/or monadic
    style.
	
	```haskell
	
	safeDiv :: Num a => a -> a -> Either Text a
	safeDiv x y = if y == 0 then (Left "Division by zero") else x / y
	
	safeDivAll :: [Int] -> Either Text Int
	safeDivAll []  = Left "Empty list of Ints"
	safeDivAll [x] = Left "Only one Int"
	safeDivAll (x:y:[]) = Right (x / y)
	safeDivAll (x:xs) = case safeDivAll xs of
	                      (Right y) -> safeDiv x y
						  error     -> error
	
	```
