module Day11.Main where

import Control.Monad.Trans.State
import Data.Char
import Data.Maybe
import Harness

parse :: String -> [[Int]]
parse s = map (map (\c -> ord c - ord '0')) $ lines s

inc :: [[Int]] -> [[Int]]
inc = map (map (+ 1))

stabiliseM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
stabiliseM f x = do
  x' <- f x
  if x == x'
    then return x
    else stabiliseM f x'

flash :: [[Int]] -> State Int [[Int]]
flash g = do
  let taggedG = zipWith (\y r -> map (\(x, v) -> ((x, y), v)) r) [0 ..] $ map (zip [0 ..]) g
  let hneighs = map (\r -> zip3 (1 : init r) r (tail r ++ [1])) g
  let neighs = zipWith3 zip3 (repeat (1, 1, 1) : init hneighs) hneighs (tail hneighs ++ [repeat (1, 1, 1)])
  let taggedN = zipWith (\y r -> map (\(x, v) -> ((x, y), v)) r) [0 ..] $ map (zip [0 ..]) neighs
  let newVal = taggedN >>= map (\((x, y), ((a, b, c), (d, e, f), (g, h, i))) -> ((x, y), e + length (filter (> 9) [a, b, c, d, e, f, g, h, i])))
  mapM
    ( mapM
        ( \((x, y), n) -> if n > 9 then modify (+ 1) >> return 0 else return $ if n == 0 then 0 else fromJust (lookup (x, y) newVal)
        )
    )
    taggedG

step :: [[Int]] -> State Int [[Int]]
step g = do
  let g' = inc g
  stabiliseM flash g'

allFlash :: [[Int]] -> State Int Int
allFlash xs =
  if all (all (== 0)) xs
    then return 0
    else do
      s <- step xs
      res <- allFlash s
      return $ res + 1

repM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repM 0 _ x = return x
repM n f x = repM (n -1) f x >>= f

p1 :: String -> Int
p1 s =
  let g = parse s
   in execState (repM 100 step g) 0

p2 :: String -> Int
p2 s =
  let g = parse s
   in evalState (allFlash g) 0

ex :: [[Int]]
ex =
  parse $
    "5483143223\n"
      ++ "2745854711\n"
      ++ "5264556173\n"
      ++ "6141336146\n"
      ++ "6357385478\n"
      ++ "4167524645\n"
      ++ "2176841721\n"
      ++ "6882881134\n"
      ++ "4846848554\n"
      ++ "5283751526"

gridPrint :: [[Int]] -> IO ()
gridPrint = mapM_ (putStrLn . (>>= show))

main :: Bool -> IO ()
main b = run 11 b p1 p2