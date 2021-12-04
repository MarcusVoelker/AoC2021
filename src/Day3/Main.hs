module Day3.Main where

import Control.Lens
import Data.Char
import Data.List
import Harness

digit :: Char -> Int
digit x = ord x - ord '0'

bval :: Bool -> Int
bval True = 1
bval _ = 0

bnum :: [Bool] -> Int
bnum = foldl (\x y -> x * 2 + bval y) 0

num :: [Int] -> Int
num = foldl (\x y -> x * 2 + y) 0

p1 :: String -> Int
p1 s =
  let ss = transpose $ map (map digit) $ lines s
      os = map sum ss
      zs = map (length (head ss) -) os
      gs = zipWith (<) zs os
      es = map not gs
   in bnum gs * bnum es

lfilter :: [[Int]] -> Int -> Int -> [Int]
lfilter [x] _ _ = x
lfilter ss idx def =
  let bs = map (!! idx) ss
      ss' = filter ((/= (length bs > 2 * sum bs)) . (== def) . (!! idx)) ss
   in lfilter ss' (idx + 1) def

p2 :: String -> Int
p2 s = (0, 1) & both %~ (num . lfilter (map (map digit) $ lines s) 0) & uncurry (*)

main :: Bool -> IO ()
main b = run 3 b p1 p2