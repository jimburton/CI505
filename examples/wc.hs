module Main
  where

import Control.Monad  

wordsInLine :: String -> String
wordsInLine = show . length . words

main2 = forever $ do  
    putStr "Enter a line: "  
    l <- getLine  
    putStrLn $ wordsInLine l

main = interact wordsInLine
