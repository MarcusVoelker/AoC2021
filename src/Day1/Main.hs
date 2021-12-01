module Main where

import Harness

p1 :: [Int] -> Int
p1 xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs

g1 :: [Int] -> Int
g1 x = sum [1 | (a, b) <- zip (tail x) x, a > b]

p2 :: [Int] -> Int
p2 xs = p1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))

g2 :: [Int] -> Int
g2 xs = p1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))

main :: IO ()
main = runList 1 p1

gmain :: IO ()
gmain = runList 1 g1