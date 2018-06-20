module Cipher
  ( encode'
    , decode'
  )
  where

import Data.Char
import Data.List

charSetSize :: Int
charSetSize = length ['a'..'z']

firstAlpha :: Char -> Int
firstAlpha c = if isLower c then ord 'a' else ord 'A'

code :: Char -> Int
code c = mod (ord c) (firstAlpha c)

rotatefwd :: Char -> Char -> Char
rotatefwd y x = if isAlpha y
                then chr ((mod (code y + code x) charSetSize) + (firstAlpha y))
                else y

rotateback :: Char -> Char -> Char
rotateback y x = if isAlpha y
                 then chr ((mod (code y - code x) charSetSize) + (firstAlpha y))
                 else y

encode' :: String -> String -> String
encode' [] _ = []
encode' _ [] = []
encode' str cipher = do
  let (a, b) = splitAt (length cipher) str
  let esubstr = zipWith rotatefwd a cipher
  esubstr ++ encode' b cipher

decode' :: String -> String -> String
decode' [] _ = []
decode' _ [] = []
decode' str cipher = do
  let (a, b) = splitAt (length cipher) str
  let dsubstr = zipWith rotateback a cipher
  dsubstr ++ decode' b cipher
