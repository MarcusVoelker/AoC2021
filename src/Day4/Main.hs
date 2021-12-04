{-# LANGUAGE TupleSections #-}

module Day4.Main where

import Control.Monad
import Data.List
import Data.Ord
import Harness

apctx :: (a -> b -> c) -> (a, b) -> (a, c)
apctx f (a, b) = (a, f a b)

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
bmark n = map (map (apctx $ \a -> (|| (a == n))))

bcheck :: [[(Int, Bool)]] -> Bool
bcheck = liftM2 (||) (any (all snd)) (any (all snd) . transpose)

count :: [[(Int, Bool)]] -> Int
count = sum . map (sum . map fst . filter (not . snd))

bSteps :: [Int] -> [[Int]] -> (Int, Int)
bSteps ns b =
  let bb = map (map (,False)) b
      bs = scanl (flip bmark) bb ns
   in apctx (\i -> (ns !! (i -2) *) . count) $ head $ dropWhile (not . bcheck . snd) $ zip [1 ..] bs

p1 :: String -> (Int, Int)
p1 s =
  let (ns, bs) = parse s
      be = map (bSteps ns) bs
   in minimumBy (comparing fst) be

p2 :: String -> (Int, Int)
p2 s =
  let (ns, bs) = parse s
      be = map (bSteps ns) bs
   in maximumBy (comparing fst) be

main :: Bool -> IO ()
main b = run 4 b p1 p2