module Day7.Main where

import Harness

parse :: [Char] -> [Int]
parse s = read ('[' : s ++ "]")

align :: (Int -> Int) -> [Int] -> Int -> Int
align f xs p = sum $ map (f . abs . (p -)) xs

p1 :: String -> Int
p1 s = let arr = parse s in minimum $ map (align id arr) [0 .. 2000]

p2 :: String -> Int
p2 s = let arr = parse s in minimum $ map (align (\x -> div (x * (x + 1)) 2) arr) [0 .. 2000]

main :: Bool -> IO ()
main b = run 7 b p1 p2