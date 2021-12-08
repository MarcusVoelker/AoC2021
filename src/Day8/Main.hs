module Day8.Main where

import Data.List
import qualified Data.Map as M
import Harness
import Text.Parsec hiding (Line)
import Text.Parsec.String

data Display = Display {codes :: [String], value :: [String]}

parser :: Parser Display
parser = do
  cod <- endBy (many1 letter) spaces
  _ <- char '|'
  _ <- char ' '
  val <- sepBy (many1 letter) spaces
  return $ Display cod val

instance Read Display where
  readsPrec = parsecToReadsPrec parser

p1 :: [Display] -> Int
p1 ds = sum $ map (\d -> length $ filter (\e -> length e < 5 || length e > 6) $ value d) ds

concatD :: [Int] -> Int
concatD [x, y, z, w] = 1000 * x + 100 * y + 10 * z + w

decode :: Display -> Int
decode d =
  let cd1 = head $ filter ((== 2) . length) (codes d)
      cd4 = head $ filter ((== 4) . length) (codes d)
      cd7 = head $ filter ((== 3) . length) (codes d)
      cd8 = head $ filter ((== 7) . length) (codes d)
      sixes = filter ((== 6) . length) (codes d)
      cd9 = head $ filter (\c -> null (cd4 \\ c)) sixes
      cd0 = head $ filter (\c -> null (cd1 \\ c) && c /= cd9) sixes
      cd6 = head $ filter (\c -> c /= cd9 && c /= cd0) sixes
      fives = filter ((== 5) . length) (codes d)
      cd5 = head $ filter (\c -> null (c \\ cd9) && null (c \\ cd6)) fives
      cd3 = head $ filter (\c -> null (c \\ cd9) && c /= cd5) fives
      cd2 = head $ filter (\c -> c /= cd3 && c /= cd5) fives
      m = M.fromList [(sort cd0, 0), (sort cd1, 1), (sort cd2, 2), (sort cd3, 3), (sort cd4, 4), (sort cd5, 5), (sort cd6, 6), (sort cd7, 7), (sort cd8, 8), (sort cd9, 9)]
   in foldl (\x y -> x * 10 + y) 0 $
        map
          ( \c ->
              m M.! (sort c)
          )
          (value d)

p2 :: [Display] -> Int
p2 ds = sum $ map decode ds

main :: Bool -> IO ()
main b = runList 8 b p1 p2