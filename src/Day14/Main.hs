module Day14.Main where

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Harness

parseRule :: String -> ((Char, Char), Char)
parseRule s = ((s !! 0, s !! 1), s !! 6)

parse :: [Char] -> (String, [((Char, Char), Char)])
parse s =
  let start = head $ lines s
      rules = map parseRule $ drop 2 $ lines s
   in (start, rules)

rep :: Int -> (a -> a) -> a -> a
rep 0 _ x = x
rep n f x = f $ rep (n -1) f x

step :: [((Char, Char), Char)] -> String -> String
step rs s = (head s :) $ zip s (tail s) >>= (\(a, b) -> maybeToList (lookup (a, b) rs) ++ [b])

initM :: String -> M.Map (Char, Char) Int
initM s = M.fromListWith (+) $ zipWith (curry (,1)) s (tail s)

stepM :: [((Char, Char), Char)] -> M.Map (Char, Char) Int -> M.Map (Char, Char) Int
stepM rs m = M.unionsWith (+) (map (\((a, b), n) -> let c = fromJust (lookup (a, b) rs) in M.fromList [((a, c), n), ((c, b), n)]) $ M.toList m)

eval :: String -> Int
eval s = (\l -> last l - head l) $ sort $ map (\c -> length (filter (== c) s)) $ nub s

evalM :: String -> M.Map (Char, Char) Int -> Int
evalM s m = (\l -> last l - head l) $ sort $ map snd $ M.toList $ M.map (`div` 2) $ M.adjust (+ 1) (last s) $ M.adjust (+ 1) (head s) $ M.unionWith (+) (M.mapKeysWith (+) fst m) (M.mapKeysWith (+) snd m)

p1 :: String -> IO ()
p1 s =
  let (st, rs) = parse s
   in print $ evalM st $ rep 10 (stepM rs) $ initM st

graph :: [(Int, Int)] -> IO ()
graph ps = mapM_ print [[if (x, y) `elem` ps then '#' else ' ' | x <- [0 .. maximum (map fst ps)]] | y <- [0 .. maximum (map snd ps)]]

p2 :: String -> IO ()
p2 s =
  let (st, rs) = parse s
   in print $ evalM st $ rep 40 (stepM rs) $ initM st

main :: Bool -> IO ()
main b = runIO 14 b p1 p2