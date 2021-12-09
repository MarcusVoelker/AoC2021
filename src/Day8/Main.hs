module Day8.Main where

import Control.Monad.Trans.State
import Data.List
import qualified Data.Map as M
import Harness
import Text.Parsec hiding (Line, State)
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
p1 ds = sum $ map (length . filter (\e -> length e < 5 || length e > 6) . value) ds

getCond :: Int -> (String -> Bool) -> State [String] String
getCond l f = do
  ls <- get
  let res = head $ filter (\c -> length c == l && f c) ls
  let rest = ls \\ [res]
  put rest
  return res

mapBuilder :: State [String] (M.Map String Int)
mapBuilder = do
  cd1 <- getCond 2 (const True)
  cd4 <- getCond 4 (const True)
  cd7 <- getCond 3 (const True)
  cd8 <- getCond 7 (const True)
  cd9 <- getCond 6 (null . (cd4 \\))
  cd0 <- getCond 6 (null . (cd1 \\))
  cd6 <- getCond 6 (const True)
  cd5 <- getCond 5 (\c -> null (c \\ cd9) && null (c \\ cd6))
  cd3 <- getCond 5 (null . (\\ cd9))
  cd2 <- getCond 5 (const True)
  return $ M.fromList [(sort cd0, 0), (sort cd1, 1), (sort cd2, 2), (sort cd3, 3), (sort cd4, 4), (sort cd5, 5), (sort cd6, 6), (sort cd7, 7), (sort cd8, 8), (sort cd9, 9)]

buildMap :: Display -> M.Map String Int
buildMap d = evalState mapBuilder (codes d)

decode :: Display -> Int
decode d =
  let m = buildMap d
   in foldl (\x y -> x * 10 + y) 0 $ map ((m M.!) . sort) (value d)

p2 :: [Display] -> Int
p2 ds = sum $ map decode ds

main :: Bool -> IO ()
main b = runList 8 b p1 p2