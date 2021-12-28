module Week2 where

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
