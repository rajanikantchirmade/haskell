module Main where

import System.Environment
import Cipher

main :: IO ()
main = do
  (command:string:key:_) <- getArgs
  case command of
    "encode" -> putStrLn (encode' string key)
    "decode" -> putStrLn (decode' string key)
    _ -> putStrLn "Usage : cipher-exe <enocde|decode> <input string> <key>"
  
