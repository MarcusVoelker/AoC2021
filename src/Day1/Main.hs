module Main where

import Harness

p1 :: [Int] -> Int
p1 xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs

p2 :: [Int] -> Int
p2 xs = p1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))

main :: IO ()
main = runList 1 p2