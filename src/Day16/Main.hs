module Day16.Main where

import Control.Lens
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Harness
import Numeric (readHex)
import Text.Parsec hiding (Line, State)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Printf (printf)

data Package = Literal {_version :: Int, _value :: Integer} | Operator {_version :: Int, _type :: Int, _arguments :: [Package]} deriving (Show)

expr :: Package -> String
expr (Literal _ v) = show v
expr (Operator _ 0 as) = "(" ++ intercalate "+" (map expr as) ++ ")"
expr (Operator _ 1 as) = "(" ++ intercalate "*" (map expr as) ++ ")"
expr (Operator _ 2 as) = "min(" ++ intercalate "," (map expr as) ++ ")"
expr (Operator _ 3 as) = "max(" ++ intercalate "," (map expr as) ++ ")"
expr (Operator _ 5 as) = "(" ++ intercalate ">" (map expr as) ++ ")"
expr (Operator _ 6 as) = "(" ++ intercalate "<" (map expr as) ++ ")"
expr (Operator _ 7 as) = "(" ++ intercalate "=" (map expr as) ++ ")"
expr _ = "error"

vSum :: Package -> Int
vSum (Literal v _) = v
vSum (Operator v _ a) = v + sum (map vSum a)

eval :: Package -> Int
eval (Literal _ v) = fromIntegral v
eval (Operator _ 0 as) = sum $ map eval as
eval (Operator _ 1 as) = product $ map eval as
eval (Operator _ 2 as) = minimum $ map eval as
eval (Operator _ 3 as) = maximum $ map eval as
eval (Operator _ 5 as) = (\(a : b : _) -> if a > b then 1 else 0) $ map eval as
eval (Operator _ 6 as) = (\(a : b : _) -> if a < b then 1 else 0) $ map eval as
eval (Operator _ 7 as) = (\(a : b : _) -> if a == b then 1 else 0) $ map eval as
eval _ = -1

readBinary :: String -> Integer
readBinary = foldl (\a b -> a * 2 + fromIntegral (ord b - ord '0')) 0

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM b m = do
  v <- b
  if v
    then m >>= (\x -> (x :) <$> whileM b m)
    else return []

literalParser :: Parser Package
literalParser = do
  v <- readBinary <$> count 3 digit
  _ <- string "100"
  res <- whileM ((== '1') <$> lookAhead anyChar) (anyChar >> count 4 digit)
  last <- anyChar >> count 4 digit
  return $ Literal (fromInteger v) (readBinary $ concat (res ++ [last]))

operatorParser :: Parser Package
operatorParser = do
  v <- readBinary <$> count 3 digit
  t <- readBinary <$> count 3 digit
  i <- (== '0') <$> anyChar
  cs <-
    if i
      then do
        ccount <- readBinary <$> count 15 anyChar
        ccode <- count (fromInteger ccount) anyChar
        either (fail . show) return (parse (many parser) "" ccode)
      else do
        ccount <- fromInteger . readBinary <$> count 11 anyChar
        count ccount parser
  return $ Operator (fromInteger v) (fromInteger t) cs

readMany :: (Read a) => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

parser :: Parser Package
parser = try literalParser <|> operatorParser

unhexD :: Char -> String
unhexD = printf "%04b" . fst . head . (readHex :: ReadS Integer) . return

unhex :: String -> String
unhex = (>>= unhexD)

p1 :: String -> Int
p1 s = either (const (-1)) vSum $ parse parser "" $ unhex s

p2 :: String -> Int
p2 s = either (const (-1)) eval $ parse parser "" $ unhex s

main :: Bool -> IO ()
main b = run 16 b p1 p2