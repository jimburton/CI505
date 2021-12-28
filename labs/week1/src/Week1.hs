module Exercises
  where

import Prelude hiding (elem, take, drop, length)

test :: String
test = "Hello, world!"

square :: Int -> Int
square x = x*x

sumsquare :: Int -> Int -> Int
sumsquare x y = square x + square y

elem :: Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = (x==y) || elem x ys

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs

take :: Int -> [a] -> [a]
take 0 xs     = []
take n []     = []
take n (x:xs) = x : take (n-1) xs
