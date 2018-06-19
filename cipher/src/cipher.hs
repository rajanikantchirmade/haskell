module Cipher
  ( encode'
    , decode'
  )
  where

import Data.Char
import Data.List

code :: Char -> Int
code c = mod (ord c)(if isLower c then 97 else 65)

rotatefwd :: Char -> Char -> Char
rotatefwd y x = if isAlpha y
                then chr ((mod (code y + code x) 26) + (if isLower y then 97 else 65))
                else y

rotateback :: Char -> Char -> Char
rotateback y x = if isAlpha y
                 then chr ((mod (code y - code x) 26) + (if isLower y then 97 else 65))
                 else y

encode' :: [Char] -> [Char] -> [Char]
encode' [] _ = []
encode' str cipher = do
  let (a, b) = splitAt (length cipher) str
  let esubstr = zipWith rotatefwd a cipher
  esubstr ++ encode' b cipher

decode' :: [Char] -> [Char] -> [Char]
decode' [] _ = []
decode' str cipher = do
  let (a, b) = splitAt (length cipher) str
  let dsubstr = zipWith rotateback a cipher
  dsubstr ++ decode' b cipher
