module Main where

import System.IO
import Data.List (sortBy)
import Log


------------------------------
-- Main function does nothing
------------------------------
main :: IO ()
main = putStrLn ""

-- | Exercise 1
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

-- | Exercise 3
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = foldl (\acc mlm -> case mlm of
                              InvalidLM _ -> acc
                              ValidLM lm  -> lm : acc) []
-- or
--validMessagesOnly [] = []
--validMessagesOnly ((InvalidLM _):xs) = validMessagesOnly xs
--validMessagesOnly ((ValidLM lm):xs) = lm : validMessagesOnly xs

-- | Exercise 4
parse :: String -> IO [LogMessage]
parse str = fmap (validMessagesOnly . map parseMessage) (readLogFile str)

-- | Exercise 5
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

-- | Exercise 8
processLogFile :: String -> String -> IO ()
processLogFile inPath outPath = do
  ms <- fmap (unlines . map (\(ts,m) -> "["++show ts++"] "++m) . whatWentWrong) (parse inPath)
  writeFile outPath ms

