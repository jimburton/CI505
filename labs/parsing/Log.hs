-- Lab exercise for CI505

module Log where

import Data.Functor ((<&>))

-- | The classification of a message
data MessageType = Info
                 | Warning
                 | Error Int    -- ^ The parameter is the severity of the error
  deriving (Show, Eq)
   -- this line allows equality comparison and printing in GHCi

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
  deriving (Show, Eq)

data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM String
  deriving (Show, Eq)

data MaybeInt = ValidInt Int
              | InvalidInt
  deriving (Show, Eq)

-- Test functions

testReadLogFile :: (String -> IO [String]) -> String -> IO ()
testReadLogFile readF str = readF str >>= mapM_ putStrLn

testParseMessage :: (String -> IO [String]) ->
                    String ->
                    (String -> MaybeLogMessage) ->
                    IO ()
testParseMessage readF path parseF = readF path >>= mapM_ (print . parseF)

testParse :: (String -> IO [LogMessage]) ->
             Int ->
             String ->
             IO ()
testParse parseF i path = parseF path >>= mapM_ print . take i 

testWhatWentWrong :: (String -> IO [LogMessage]) ->
                     String ->
                    ([LogMessage] -> [String]) ->
                     IO ()
testWhatWentWrong parseF path wwwF = parseF path >>= print . wwwF

{- Monadic versions for comparison -}
testReadLogFile' :: (String -> IO [String]) -> String -> IO ()
testReadLogFile' readF str = readF str >>= mapM_ putStrLn

testParseMessage' :: (String -> IO [String]) ->
                    String ->
                    (String -> MaybeLogMessage) ->
                    IO ()
testParseMessage' readF path parseF = readF path >>= mapM_ (print. parseF) 

testParse' :: (String -> IO [LogMessage]) ->
             Int ->
             String ->
             IO ()
testParse' parseF i path = parseF path >>= mapM_ print . take i

testWhatWentWrong' :: (String -> IO [LogMessage]) ->
                     String ->
                    ([LogMessage] -> [(TimeStamp, String)]) ->
                     IO ()
testWhatWentWrong' parseF path wwwF = parseF path >>= print . wwwF
                           
