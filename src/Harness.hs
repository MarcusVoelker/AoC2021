module Harness where

import Text.Parsec
import Text.Parsec.String

run :: (Show a) => Int -> (String -> a) -> IO ()
run idx func = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  print (func f)

runList :: (Show a, Read b) => Int -> ([b] -> a) -> IO ()
runList idx func = run idx (func . map read . lines)

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = (,) <$> p <*> getInput

parsecToReadsPrec :: Parser a -> Int -> ReadS a
parsecToReadsPrec parsecParser prec input =
  case parse (withRemaining parsecParser) "" input of
    Left _ -> []
    Right result -> [result]