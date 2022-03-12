{-# Language OverloadedStrings #-}
module Main
  where

import Control.Monad (forever)
import qualified Network.WebSockets as WS
import Data.Text (Text) 

main :: IO ()
main = do
  putStrLn "starting echo on port 9160"
  WS.runServer "127.0.0.1" 9160 wsapp

wsapp :: WS.ServerApp
wsapp pending = WS.acceptRequest pending >>= talk

talk :: WS.Connection -> IO ()
talk conn = forever $ do
  msg <- WS.receiveData conn
  WS.sendTextData conn $ echo msg

echo :: Text -> Text
echo msg = "echo: " <> msg
