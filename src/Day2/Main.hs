{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
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

exec :: (Int -> a -> a) -> (Int -> a -> a) -> (Int -> a -> a) -> (a -> b) -> a -> [Move] -> b
exec f g h c x =
  c
    . foldl
      ( \a m -> case m of
          Forward n -> f n a
          Up n -> g n a
          Down n -> h n a
      )
      x

data State1 = State1 {state1X :: Int, state1D :: Int} deriving (Show)

data State2 = State2 {state2X :: Int, state2D :: Int, state2A :: Int} deriving (Show)

makeFields ''State1
makeFields ''State2

res :: (Num b, HasX a b, HasD a b) => a -> b
res = (*) <$> view x <*> view d

p1 :: [Move] -> Int
p1 = exec (x +~) (d -~) (d +~) res $ State1 0 0

p2 :: [Move] -> Int
p2 = exec (\n t -> t &~ (x += n >> d += (n * view a t))) (a -~) (a +~) res $ State2 0 0 0

main :: IO ()
main = runList 2 p2