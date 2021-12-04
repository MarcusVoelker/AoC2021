{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Data.List
import Data.Ord
import Harness

testIn = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = take n xs : splitN n (drop n xs)

parse :: String -> ([Int], [[[Int]]])
parse s =
  let ls = lines s
      ns = read ("[" ++ head ls ++ "]") :: [Int]
      bs = map (map (map read . splitN 3) . tail) $ splitN 6 $ tail ls
   in (ns, bs)

bmark :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
bmark n b = map (map (\(a, t) -> (a, if a == n then True else t))) b

bcheck :: [[(Int, Bool)]] -> Bool
bcheck bb = any (all snd) bb || any (all snd) (transpose bb)

count :: [[(Int, Bool)]] -> Int
count bb = sum $ map (sum . map fst . filter (not . snd)) bb

bSteps :: [Int] -> [[Int]] -> (Int, Int)
bSteps ns b =
  let bb = map (map (,False)) b
      bs = scanl (flip bmark) bb ns
   in (\(i, b) -> (i, count b * (ns !! (i -2)))) $ head $ dropWhile (not . bcheck . snd) $ zip [1 ..] bs

p1 :: String -> (Int, Int)
p1 s =
  let (ns, bs) = parse s
      be = map (bSteps ns) bs
   in minimumBy (comparing (^. _1)) be

p2 :: String -> (Int, Int)
p2 s =
  let (ns, bs) = parse s
      be = map (bSteps ns) bs
   in maximumBy (comparing (^. _1)) be

main :: IO ()
main = run 4 p2