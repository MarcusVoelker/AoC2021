module Day13.Main where

import Data.Bifunctor
import Data.List
import Harness

data Fold = H Int | V Int

parseFold :: String -> Fold
parseFold s =
  let s' = drop 11 s
      h = (== 'x') $ head s'
      s'' = drop 2 s'
   in (if h then H else V) (read s'')

parse :: [Char] -> ([(Int, Int)], [Fold])
parse s =
  let dots = takeWhile (',' `elem`) $ lines s
      folds = tail $ dropWhile (',' `elem`) $ lines s
      dot = map (\s -> (read (takeWhile (/= ',') s), read $ tail $ dropWhile (/= ',') s)) dots
      fold = map parseFold folds
   in (dot, fold)

doFold :: Fold -> [(Int, Int)] -> [(Int, Int)]
doFold (H x) ps = map (first ((2 * x) -)) ps
doFold (V y) ps = map (second ((2 * y) -)) ps

makeFold :: Fold -> [(Int, Int)] -> [(Int, Int)]
makeFold (H x) ps = nub $ filter ((< x) . fst) ps ++ doFold (H x) (filter ((> x) . fst) ps)
makeFold (V y) ps = nub $ filter ((< y) . snd) ps ++ doFold (V y) (filter ((> y) . snd) ps)

p1 :: String -> IO ()
p1 s =
  let (ps, fs) = parse s
   in print $ length $ makeFold (head fs) ps

graph :: [(Int, Int)] -> IO ()
graph ps = mapM_ print [[if (x, y) `elem` ps then '#' else ' ' | x <- [0 .. maximum (map fst ps)]] | y <- [0 .. maximum (map snd ps)]]

p2 :: String -> IO ()
p2 s =
  let (ps, fs) = parse s
   in graph $ foldl (flip makeFold) ps fs

main :: Bool -> IO ()
main b = runIO 13 b p1 p2