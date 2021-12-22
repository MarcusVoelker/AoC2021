module Day21.Main where

import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Harness

input :: (Integer, Integer)
input = (6, 9)

stepP :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
stepP (p, s, d) =
  let rs = d + mod (d + 1) 100 + mod (d + 2) 200
      d' = mod (d + 2) 100 + 1
      p' = mod (p + rs - 1) 10 + 1
      s' = s + p'
   in (p', s', d')

step :: (Bool, (Integer, Integer), (Integer, Integer), Integer, Integer) -> (Bool, (Integer, Integer), (Integer, Integer), Integer, Integer)
step (False, (p1, p2), (s1, s2), d, c) = let (p', s', d') = stepP (p1, s1, d) in (True, (p', p2), (s', s2), d', c + 1)
step (True, (p1, p2), (s1, s2), d, c) = let (p', s', d') = stepP (p2, s2, d) in (False, (p1, p'), (s1, s'), d', c + 1)

p1 :: (Integer, Integer) -> (((Integer, Integer), (Integer, Integer), Integer, Integer), Integer)
p1 (l1, l2) =
  let (_, p, (s1, s2), d, c) = head $ dropWhile (\(_, _, (a, b), _, _) -> a < 1000 && b < 1000) $ iterate step (False, (l1, l2), (0, 0), 1, 0)
   in ((p, (s1, s2), d, c), min s1 s2 * c * 3)

rolls :: [(Integer, Integer)]
rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

dStep :: (Bool, M.Map ((Integer, Integer), (Integer, Integer)) Integer) -> (Bool, M.Map ((Integer, Integer), (Integer, Integer)) Integer)
dStep (False, m) =
  let ms = M.unionsWith (+) $ map (\(r, cc) -> M.map (* cc) $ M.mapKeys (\((a, b), (c, d)) -> ((mod (a + r -1) 10 + 1, b), (c + mod (a + r -1) 10 + 1, d))) m) rolls
   in (True, ms)
dStep (True, m) =
  let ms = M.unionsWith (+) $ map (\(r, cc) -> M.map (* cc) $ M.mapKeys (\((a, b), (c, d)) -> ((a, mod (b + r -1) 10 + 1), (c, d + mod (b + r -1) 10 + 1))) m) rolls
   in (False, ms)

dRStep :: (Bool, M.Map ((Integer, Integer), (Integer, Integer)) Integer) -> (Bool, M.Map ((Integer, Integer), (Integer, Integer)) Integer)
dRStep (p, m) =
  let (p', m') = dStep (p, m)
      (undone, done) = partition (\((a, b), (c, d)) -> c < 21 && d < 21) $ M.keys m'
      (awin, bwin) = partition (\((_, b), (_, d)) -> b > 21) done
      act = sum $ map (m' M.!) awin
      bct = sum $ map (m' M.!) bwin
      m'' = foldr M.delete m' done
   in (p', m'')

dRun :: (Bool, M.Map ((Integer, Integer), (Integer, Integer)) Integer) -> (Integer, Integer)
dRun (p, m)
  | M.null m = (0, 0)
  | otherwise =
    let (p', m') = dStep (p, m)
        (undone, done) = partition (\((a, b), (c, d)) -> c < 21 && d < 21) $ M.keys m'
        (awin, bwin) = partition (\((_, _), (c, d)) -> c >= 21) done
        act = sum $ map (m' M.!) awin
        bct = sum $ map (m' M.!) bwin
        m'' = foldr M.delete m' done
        (ra, rb) = dRun (p', m'')
     in (ra + act, rb + bct)

p2 :: (Integer, Integer) -> Integer
p2 p = uncurry max $ dRun (False, M.singleton (p, (0, 0)) 1)

main :: Bool -> IO ()
main False = print $ p1 input
main True = print $ p2 input