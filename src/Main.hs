module Main where

import qualified Day1.Main as D1
import qualified Day10.Main as D10
import qualified Day11.Main as D11
import qualified Day12.Main as D12
import qualified Day13.Main as D13
import qualified Day14.Main as D14
import qualified Day15.Main as D15
import qualified Day16.Main as D16
import qualified Day2.Main as D2
import qualified Day3.Main as D3
import qualified Day4.Main as D4
import qualified Day5.Main as D5
import qualified Day6.Main as D6
import qualified Day7.Main as D7
import qualified Day8.Main as D8
import qualified Day9.Main as D9

ms :: [Bool -> IO ()]
ms = [D1.main, D2.main, D3.main, D4.main, D5.main, D6.main, D7.main, D8.main, D9.main, D10.main, D11.main, D12.main, D13.main, D14.main, D15.main, D16.main]

main :: IO ()
main = do
  putStrLn "Choose Day and Star, please"
  day <- readLn :: IO Int
  star <- (== 2) <$> (readLn :: IO Int)
  (ms !! (day - 1)) star