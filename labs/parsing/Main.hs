module Main where

import System.IO
import Data.List (sortBy)
import Log


------------------------------
-- Main function does nothing
------------------------------
main :: IO ()
main = putStrLn ""

{- | Exercise 1

     We could have done this in a do block without using fmap:

     do str <- readFile path
        return (lines str)

     We could have made the existing version "uncurried" by leaving out
     the argument:

     readLogFile = fmap lines . readFile 
-}
readLogFile :: FilePath -> IO [String]
readLogFile path = fmap lines (readFile path)
          
-- | Exercise 2
parseMessage :: String -> MaybeLogMessage
parseMessage str = let wds = words str in
  case head wds of
    "I" -> let ts  = read (wds !! 1) :: Int
               msg = concat (drop 2 wds) in
             ValidLM (LogMessage Info ts msg)
    "W" -> let ts  = read (wds !! 1) :: Int
               msg = concat (drop 2 wds) in
             ValidLM (LogMessage Warning ts msg)
    "E" -> let sev = read (wds !! 1) :: Int
               ts  = read (wds !! 2) :: Int
               msg = concat (drop 3 wds) in
      ValidLM (LogMessage (Error sev) ts msg)
    _ -> InvalidLM str

{- | Exercise 3

     Note that the definition is uncurried, as are several of the
     others below. There is no need to name the list of log messages,
     as it will just be supplied as the rightmost argument to the call to foldl.

     We could have done this without a fold:

     validMessagesOnly [] = []
     validMessagesOnly ((InvalidLM _):xs) = validMessagesOnly xs
     validMessagesOnly ((ValidLM lm):xs) = lm : validMessagesOnly xs
-}
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = foldl (\acc mlm -> case mlm of
                              InvalidLM _ -> acc
                              ValidLM lm  -> lm : acc) []

-- | Exercise 4
parse :: String -> IO [LogMessage]
parse = fmap (validMessagesOnly . map parseMessage) . readLogFile 

{- | Exercise 5

     We could have done this with nested if statements that compare ts1 and ts2 and
     return LT, EQ or GT as appropriate, but the timestamps are ints, which are in
     the Ord typeclass so we can just call compare on them.
-}
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 `compare` ts2

-- | Exercise 6
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- | Exercise 7
whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
whatWentWrong =
  map (\(LogMessage _ ts m) -> (ts,m))
  . sortMessages . foldl (\acc lm ->
                             case lm of
                               (LogMessage (Error sev) _ _) ->
                                 if sev >= 50
                                 then lm : acc
                                 else acc
                               _  -> acc) []

{- | Exercise 8

     There are various alternative ways of writing this, and the
     best one for you is the one you find most clear.

    E.g., using fmap for all the pure functions:

    processLogFile inPath outPath = do
      ms <- fmap (unlines . map (\(ts,m) -> "["++show ts++"] "++m) . whatWentWrong) (parse inPath)
      writeFile outPath ms

    Or without a do block at all and using (>>=) to pass the output from
    one IO action to another. Note that this function is a single expression
    and could be on one line but I broke it over two lines to make it easier to read:

    processLogFile inPath outPath = 
      fmap (unlines . map (\(ts,m) -> "["++show ts++"] "++m) . whatWentWrong) (parse inPath)
      >>= writeFile outPath 

-}
processLogFile :: String -> String -> IO ()
processLogFile inPath outPath = do
  lms <- parse inPath
  let worst = whatWentWrong lms
      formatted = map (\(ts,m) -> "["++show ts++"] "++m) worst
  writeFile outPath (unlines formatted)
