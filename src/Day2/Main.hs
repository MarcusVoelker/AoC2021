{-# LANGUAGE TupleSections #-}

module Main where

import Harness
import Text.Parsec
import Text.Parsec.String

data Move = Forward Int | Up Int | Down Int deriving (Show)

instance Read Move where
  readsPrec = parsecToReadsPrec parser

parser :: Parser Move
parser =
  (string "forward " >> (Forward . read <$> many1 digit))
    <|> (string "up " >> (Up . read <$> many1 digit))
    <|> (string "down " >> (Down . read <$> many1 digit))

p1 :: [Move] -> Int
p1 =
  uncurry (*)
    . foldr
      ( \m (x, d) -> case m of
          Forward y -> (x + y, d)
          Up y -> (x, d - y)
          Down y -> (x, d + y)
      )
      (0, 0)

p2 :: [Move] -> Int
p2 =
  (\(a, b, _) -> a * b)
    . foldl
      ( \(x, d, a) m -> case m of
          Forward y -> (x + y, d + y * a, a)
          Up y -> (x, d, a - y)
          Down y -> (x, d, a + y)
      )
      (0, 0, 0)

main :: IO ()
main = runList 2 p2