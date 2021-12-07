module Day6.Main where

import qualified Data.Map as M
import Harness

parse :: [Char] -> [Int]
parse s = read ('[' : s ++ "]")

mParse :: [Char] -> M.Map Int Integer
mParse s =
  let l = parse s
   in M.fromList $ map (\i -> (i, fromIntegral $ length $ filter (== i) l)) [0 .. 8]

mStep :: M.Map Int Integer -> M.Map Int Integer
mStep m =
  let m' = M.mapKeys (subtract 1) m
      m'' = M.insertWith (+) 6 (m' M.! (-1)) m'
      m''' = M.insert 8 (m' M.! (-1)) m''
   in M.delete (-1) m'''

rep :: Int -> (a -> a) -> a -> a
rep 0 _ x = x
rep n f x = f $ rep (n -1) f x

p1 :: String -> Integer
p1 = M.foldr (+) 0 . rep 80 mStep . mParse

p2 :: String -> Integer
p2 = M.foldr (+) 0 . rep 256 mStep . mParse

main :: Bool -> IO ()
main b = run 6 b p1 p2