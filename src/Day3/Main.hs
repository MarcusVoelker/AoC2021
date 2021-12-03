module Main where

import Data.Char
import Data.List
import Harness

digit :: Char -> Int
digit x = ord x - ord '0'

bdigit :: Char -> Bool
bdigit x = x == '1'

bval :: Bool -> Int
bval True = 1
bval _ = 0

bnum :: [Bool] -> Int
bnum = foldl (\x y -> x * 2 + bval y) 0

p1 :: String -> Int
p1 s =
  let ss = transpose $ map (map bdigit) $ lines s
      zs = map (length . filter not) ss
      os = map (length . filter id) ss
      gs = zipWith (<) zs os
      es = map not gs
   in bnum gs * bnum es

lfilter :: [[Bool]] -> Bool -> Int -> Bool -> [Bool]
lfilter ss mc idx def =
  let bs = map (!! idx) ss
      zs = length $ filter not bs
      os = length $ filter id bs
      ss'
        | zs == os = filter ((== def) . (!! idx)) ss
        | (zs > os) == mc = filter (not . (!! idx)) ss
        | otherwise = filter (!! idx) ss
   in if length ss' == 1 then head ss' else lfilter ss' mc (idx + 1) def

p2 :: String -> Int
p2 s =
  let ss = map (map bdigit) $ lines s
      os = lfilter ss True 0 True
      cs = lfilter ss False 0 False
      or = bnum os
      cr = bnum cs
   in or * cr

main :: IO ()
main = run 3 p2