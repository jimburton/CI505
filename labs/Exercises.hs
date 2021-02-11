module Exercises
  where

test :: String
test = "Hello, world!"

square :: Int -> Int
square x = x*x

sumsquare :: Int -> Int -> Int
sumsquare x y = square x + square y

memb :: Eq a => [a] -> a -> Bool
memb [] x     = False
memb (y:ys) x = (x==y) || memb ys x

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop (n-1) xs

take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n []     = []
take' n (x:xs) = x : take (n-1) xs
