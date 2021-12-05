module Day5.Main where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord
import Harness
import Text.Parsec hiding (Line)
import Text.Parsec.String

data Line = Line {lineStart :: (Int, Int), lineEnd :: (Int, Int)} deriving (Show, Eq)

parser :: Parser Line
parser = do
  startX <- read <$> many1 digit
  _ <- char ','
  startY <- read <$> many1 digit
  _ <- string " -> "
  endX <- read <$> many1 digit
  _ <- char ','
  endY <- read <$> many1 digit
  return $ Line (startX, startY) (endX, endY)

instance Read Line where
  readsPrec = parsecToReadsPrec parser

axis :: Int -> Int -> [Int]
axis a b
  | a <= b = [a .. b]
  | otherwise = reverse [b .. a]

mZip :: [a] -> [b] -> [(a, b)]
mZip [a] b = map (a,) b
mZip a [b] = map (,b) a
mZip a b = zip a b

traceLine :: Line -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
traceLine (Line (startX, startY) (endX, endY)) m =
  foldl (\m' (x, y) -> M.insertWith (+) (x, y) 1 m') m $ mZip (axis startX endX) (axis startY endY)

p1 :: [Line] -> Int
p1 s = M.size $ M.filter (> 1) $ foldr traceLine M.empty $ filter (\(Line (xs, ys) (xe, ye)) -> xs == xe || ys == ye) s

p2 :: [Line] -> Int
p2 s = M.size $ M.filter (> 1) $ foldr traceLine M.empty s

main :: Bool -> IO ()
main b = runList 5 b p1 p2