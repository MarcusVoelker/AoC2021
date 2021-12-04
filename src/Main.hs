module Main where

import qualified Day1.Main as D1
import qualified Day2.Main as D2
import qualified Day3.Main as D3
import qualified Day4.Main as D4

ms :: [Bool -> IO ()]
ms = [D1.main, D2.main, D3.main, D4.main]

main :: IO ()
main = do
  putStrLn "Choose Day and Star, please"
  day <- readLn :: IO Int
  star <- (== 2) <$> (readLn :: IO Int)
  (ms !! (day - 1)) star