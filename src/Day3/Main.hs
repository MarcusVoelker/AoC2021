module Main where

import Data.List
import Harness

p1 :: String -> Int
p1 s =
  let ss = transpose $ lines s
      zs = map (length . filter (== '0')) ss
      os = map (length . filter (== '1')) ss
      gs = zipWith (\a b -> if a > b then 0 else 1) zs os
      es = map (1 -) gs
      gr = foldl (\a b -> a * 2 + b) 0 gs
      er = foldl (\a b -> a * 2 + b) 0 es
   in gr * er

lfilter :: [String] -> Bool -> Int -> Char -> String
lfilter ss mc idx def =
  let bs = map (!! idx) ss
      zs = length $ filter (== '0') bs
      os = length $ filter (== '1') bs
      ss'
        | zs == os = filter ((== def) . (!! idx)) ss
        | (zs > os) == mc = filter ((== '0') . (!! idx)) ss
        | otherwise = filter ((== '1') . (!! idx)) ss
   in if length ss' == 1 then head ss' else lfilter ss' mc (idx + 1) def

p2 :: String -> Int
p2 s =
  let ss = lines s
      os = lfilter ss True 0 '1'
      cs = lfilter ss False 0 '0'
      or = foldl (\a b -> a * 2 + b) 0 $ map (\c -> if c == '0' then 0 else 1) os
      cr = foldl (\a b -> a * 2 + b) 0 $ map (\c -> if c == '0' then 0 else 1) cs
   in or * cr

main :: IO ()
main = run 3 p2