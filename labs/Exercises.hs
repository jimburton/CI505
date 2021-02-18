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

--------------------------
-- Week 2
--------------------------

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\ys -> (length ys, head ys)) (pack xs)

decode :: Eq a => [(Int, a)] -> [a]
decode xs = concatMap (\(i, c) -> replicate i c) xs

data RLE a = Single a | Multiple Int a deriving Show

encodeRLE :: Eq a => [a] -> [RLE a]
encodeRLE xs = map (\(i, c) -> if i == 1 then Single c
                     else Multiple i c) (encode xs)

decodeRLE :: Eq a => [RLE a] -> [a]
decodeRLE xs = concatMap (\r -> case r of
                             Single c     -> [c]
                             Multiple i c -> replicate i c) xs
