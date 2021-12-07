module Day7.Main where

import Harness

parse :: [Char] -> [Int]
parse s = read ('[' : s ++ "]")

align :: Int -> [Int] -> Int
align p xs = sum $ map (abs . (p -)) xs

alignSq :: Int -> [Int] -> Int
alignSq p xs = sum $ map ((\x -> div (x * (x + 1)) 2) . abs . (p -)) xs

p1 :: String -> Int
p1 s = let arr = parse s in minimum $ map (\p -> align p arr) [0 .. 2000]

p2 :: String -> Int
p2 s = let arr = parse s in minimum $ map (\p -> alignSq p arr) [0 .. 2000]

main :: Bool -> IO ()
main b = run 7 b p1 p2