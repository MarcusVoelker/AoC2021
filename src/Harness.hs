module Harness where

run :: (Show a) => Int -> (String -> a) -> IO ()
run idx func = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  print (func f)

runList :: (Show a, Read b) => Int -> ([b] -> a) -> IO ()
runList idx func = run idx (func . map read . lines)