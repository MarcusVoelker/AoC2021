module Day15.Main where

import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord
import Harness

parse :: String -> [[Int]]
parse s = map (\l -> read ('[' : intersperse ',' l ++ "]")) $ lines s

neighs :: [[a]] -> (Int, Int) -> [(Int, Int)]
neighs g (x, y) = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length (head g) && y' < length g) [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

pathStep :: [[Int]] -> (M.Map (Int, Int) Int, [(Int, Int)]) -> IO (M.Map (Int, Int) Int, [(Int, Int)])
pathStep _ (m, []) = return (m, [])
pathStep g (m, l : ls) = do
  putStrLn $ show (l : ls)
  let ns = neighs g l
  return $ foldr (\(x, y) (m, ls) -> if (x, y) `M.notMember` m || m M.! (x, y) > m M.! l + (g !! (snd l) !! (fst l)) then (M.insert (x, y) (m M.! l + (g !! (snd l) !! (fst l))) m, if (x, y) `notElem` ls then (x, y) : ls else ls) else (m, ls)) (m, ls) ns

optStep :: [[Int]] -> (M.Map (Int, Int) Int, [(Int, Int)]) -> IO (M.Map (Int, Int) Int, [(Int, Int)])
optStep _ (m, []) = return (m, [])
optStep g (m, l : ls)
  | (0, 0) `M.member` m = return (m, l : ls)
  | otherwise = do
    let ns = neighs g l
    let res = foldr (\(x, y) (m, ls) -> if (x, y) `M.notMember` m || m M.! (x, y) > m M.! l + (g !! snd l !! fst l) then (M.insert (x, y) (m M.! l + (g !! snd l !! fst l)) m, if (x, y) `notElem` ls then (x, y) : ls else ls) else (m, ls)) (m, ls) ns
    return $ second (sortOn (fst res M.!)) res

stabilise :: (Eq a) => (a -> a) -> a -> a
stabilise f x
  | x == f x = x
  | otherwise = stabilise f (f x)

stabiliseM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
stabiliseM f x = do
  x' <- f x
  if x == x'
    then return x
    else stabiliseM f x'

spath :: [[Int]] -> IO Int
spath is = do
  let exit = (length (head is) -1, length is -1)
  let start = (M.singleton exit 0, [exit])
  (dists, _) <- stabiliseM (optStep is) start
  return (dists M.! (0, 0))

embiggen :: [[Int]] -> [[Int]]
embiggen is =
  let rext = map (\r -> r ++ map (\n -> mod n 9 + 1) r ++ map (\n -> mod (n + 1) 9 + 1) r ++ map (\n -> mod (n + 2) 9 + 1) r ++ map (\n -> mod (n + 3) 9 + 1) r) is
   in rext ++ map (map (\n -> mod n 9 + 1)) rext ++ map (map (\n -> mod (n + 1) 9 + 1)) rext ++ map (map (\n -> mod (n + 2) 9 + 1)) rext ++ map (map (\n -> mod (n + 3) 9 + 1)) rext

p1 :: String -> IO ()
p1 s = spath (parse s) >>= print

p2 :: String -> IO ()
p2 s = spath (embiggen $ parse s) >>= print

main :: Bool -> IO ()
main b = runIO 15 b p1 p2