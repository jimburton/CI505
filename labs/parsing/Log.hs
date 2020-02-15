-- Lab exercise for CI505

module Log where

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
testReadLogFile readF str = do ls <- readF str
                               mapM_ putStrLn ls

testParseMessage :: (String -> IO [String]) ->
                    String ->
                    (String -> MaybeLogMessage) ->
                    IO ()
testParseMessage readF path parseF = do ls <- readF path
                                        mapM_ (putStrLn . show . parseF) ls

testParse :: (String -> IO [LogMessage]) ->
             Int ->
             String ->
             IO ()
testParse parseF i path = do ls <- parseF path
                             mapM_ (putStrLn . show) (take i ls)

testWhatWentWrong :: (String -> IO [LogMessage]) ->
                     String ->
                    ([LogMessage] -> [String]) ->
                     IO ()
testWhatWentWrong parseF path wwwF = do ls <- parseF path
                                        let strs = wwwF ls 
                                        print strs

{- Monadic versions for comparison -}
testReadLogFile' :: (String -> IO [String]) -> String -> IO ()
testReadLogFile' readF str = readF str >>= mapM_ putStrLn

testParseMessage' :: (String -> IO [String]) ->
                    String ->
                    (String -> MaybeLogMessage) ->
                    IO ()
testParseMessage' readF path parseF = readF path >>=
                                      mapM_ (putStrLn . show . parseF) 

testParse' :: (String -> IO [LogMessage]) ->
             Int ->
             String ->
             IO ()
testParse' parseF i path = parseF path >>=
                           mapM_ (putStrLn . show) . (take i)

testWhatWentWrong' :: (String -> IO [LogMessage]) ->
                     String ->
                    ([LogMessage] -> [(TimeStamp, String)]) ->
                     IO ()
testWhatWentWrong' parseF path wwwF = parseF path >>= (return . wwwF) >>= print
                           
