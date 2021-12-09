module Day9.Main where

import Data.Char
import Data.List
import qualified Data.Map as M
import Harness

parse :: String -> [[Int]]
parse s = map (map (\c -> ord c - ord '0')) $ lines s

p1 :: String -> Int
p1 s =
  let g = parse s
      hneighs = map (\r -> zip3 (10 : init r) r (tail r ++ [10])) g
      neighs = zipWith3 (\r0 r1 r2 -> zip3 r0 r1 r2) ((repeat (10, 10, 10)) : init hneighs) hneighs (tail hneighs ++ [repeat (10, 10, 10)])
      lows = map (filter (\((a, b, c), (d, e, f), (g, h, i)) -> e < b && e < d && e < f && e < h)) neighs
   in sum $ map (sum . map (\((a, b, c), (d, e, f), (g, h, i)) -> e + 1)) lows

get :: [[((Int, Int), v)]] -> (Int, Int) -> v
get field (x, y) = snd (field !! y !! x)

explore :: [[((Int, Int), ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int)))]] -> [(Int, Int)] -> M.Map (Int, Int) (Int, Int)
explore field [] = M.empty
explore field ((x, y) : xs) =
  let ((_, b, _), (d, e, f), (_, h, _)) = get field (x, y)
      xs' =
        [(x, y -1) | (b < 9) && (b > e) && not (elem (x, y -1) xs)]
          ++ [(x, y + 1) | (h < 9) && (h > e) && not (elem (x, y + 1) xs)]
          ++ [(x -1, y) | (d < 9) && (d > e) && not (elem (x -1, y) xs)]
          ++ [(x + 1, y) | (f < 9) && (f > e) && not (elem (x + 1, y) xs)]
      c = explore field (xs' ++ xs)
   in foldr (\(x', y') m -> M.insert (x', y') (x, y) m) c xs'

flow :: [[Int]] -> ([(Int, Int)], M.Map (Int, Int) (Int, Int))
flow g =
  let hneighs = map (\r -> zip3 (10 : init r) r (tail r ++ [10])) g
      neighs = zipWith3 (\r0 r1 r2 -> zip3 r0 r1 r2) ((repeat (10, 10, 10)) : init hneighs) hneighs (tail hneighs ++ [repeat (10, 10, 10)])
      tagged = zipWith (\y r -> map (\(x, v) -> ((x, y), v)) r) [0 ..] $ map (zip [0 ..]) neighs
      lows = map (filter (\(_, ((a, b, c), (d, e, f), (g, h, i))) -> e < b && e < d && e < f && e < h)) tagged
   in (lows >>= map fst, explore tagged $ lows >>= map fst)

invert :: (Ord b) => M.Map a b -> M.Map b [a]
invert = M.foldrWithKey (\a b -> M.insertWith (++) b [a]) M.empty

count :: (Ord a) => M.Map a [a] -> a -> Int
count m i = 1 + sum (map (count m) $ M.findWithDefault [] i m)

p2 :: String -> Int
p2 s =
  let (bs, f') = flow (parse s)
      f = invert f'
      ss = map (count f) bs
   in (\(a : b : c : _) -> a * b * c) $ reverse $ sort ss

main :: Bool -> IO ()
main b = run 9 b p1 p2