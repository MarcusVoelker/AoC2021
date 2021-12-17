module Day1.Main where

import Harness

p1 :: [Int] -> Int
p1 xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs

p2 :: [Int] -> Int
p2 xs = p1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))

main :: Bool -> IO ()
main b = runList 1 b p1 p2