module Main where

import Harness

p1 = const "Nobody here but us chickens"

p2 = const "Nobody here but us chickens"

main :: IO ()
main = run 2 p1