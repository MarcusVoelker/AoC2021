module Harness where

import Text.Parsec
import Text.Parsec.String

run :: (Show a) => Int -> Bool -> (String -> a) -> (String -> a) -> IO ()
run idx b p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  print ((if b then p2 else p1) f)

runList :: (Show a, Read b) => Int -> Bool -> ([b] -> a) -> ([b] -> a) -> IO ()
runList idx b p1 p2 = run idx b (p1 . map read . lines) (p2 . map read . lines)

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = (,) <$> p <*> getInput

parsecToReadsPrec :: Parser a -> Int -> ReadS a
parsecToReadsPrec parsecParser prec input =
  case parse (withRemaining parsecParser) "" input of
    Left _ -> []
    Right result -> [result]