module Day22.Main where

import Data.Maybe
import qualified Data.Set as S
import Harness
import Text.Parsec hiding (Line, State)
import Text.Parsec.String

data Cube = Cube Bool (Int, Int) (Int, Int) (Int, Int) deriving (Show)

number :: Parser Int
number = do
  sgn <- optionMaybe $ char '-'
  let sign = if isNothing sgn then 1 else -1
  digits <- read <$> many1 digit
  return $ sign * digits

parser :: Parser Cube
parser = do
  _ <- char 'o'
  res <- string "n " <|> string "ff "
  let b = res == "n "
  _ <- string "x="
  xm <- number
  _ <- string ".."
  xp <- number
  _ <- string ",y="
  ym <- number
  _ <- string ".."
  yp <- number
  _ <- string ",z="
  zm <- number
  _ <- string ".."
  zp <- number
  return $ Cube b (xm, xp) (ym, yp) (zm, zp)

instance Read Cube where
  readsPrec = parsecToReadsPrec parser

bruteStep :: Cube -> S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
bruteStep (Cube b (xm, xp) (ym, yp) (zm, zp)) s = foldr (if b then S.insert else S.delete) s [(x, y, z) | x <- [xm .. xp], y <- [ym .. yp], z <- [zm .. zp]]

p1 :: [Cube] -> Integer
p1 cs = fromIntegral $ S.size $ foldl (flip bruteStep) S.empty $ take 20 cs

p2 :: [Cube] -> Integer
p2 = undefined

main :: Bool -> IO ()
main b = runList 22 b p1 p2