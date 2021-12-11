module Day10.Main where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Either
import Data.List
import Harness

scoreCorr :: Char -> Int
scoreCorr ')' = 3
scoreCorr ']' = 57
scoreCorr '}' = 1197
scoreCorr '>' = 25137

scoreIncm :: Char -> Int
scoreIncm ')' = 1
scoreIncm ']' = 2
scoreIncm '}' = 3
scoreIncm '>' = 4

pair :: Char -> Char
pair '(' = ')'
pair '[' = ']'
pair '{' = '}'
pair '<' = '>'

isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

whileA :: (Monad m) => m Bool -> m a -> m [a]
whileA cond body = do
  c <- cond
  if c
    then do
      a <- body
      (a :) <$> whileA cond body
    else return []

data Res = Corr Int | Incm Int | OK deriving (Show)

fold :: [Res] -> Res
fold [] = OK
fold (Corr x : _) = Corr x
fold [Incm x] = Incm x
fold (OK : xs) = fold xs
fold (Incm x : xs) = fold xs

lineScore :: State String Res
lineScore = do
  code <- get
  case code of
    [] -> return (Incm 0)
    [c] -> put "" >> return (if isOpen c then Incm (scoreIncm (pair c)) else Corr (scoreCorr c))
    (c : cs) -> do
      put cs
      sc <- fold <$> whileA (get >>= (\s -> return $ not (null s) && isOpen (head s))) lineScore
      rest <- do
        code' <- get
        if null code'
          then return $ Incm (scoreIncm $ pair c)
          else do
            put (tail code')
            if pair c == head code'
              then return OK
              else return $ Corr $ scoreCorr (head code')
      return $ case (sc, rest) of
        (Corr x, _) -> Corr x
        (_, Corr y) -> Corr y
        (Incm x, OK) -> Incm x
        (OK, Incm y) -> Incm y
        (Incm x, Incm y) -> Incm (x * 5 + y)
        _ -> OK

corr :: Res -> Int
corr (Corr x) = x
corr _ = 0

incm :: Res -> Int
incm (Incm x) = x
incm _ = 0

median :: [a] -> a
median [x] = x
median xs = median (tail (init xs))

p1 :: String -> Int
p1 s = sum $ map corr $ map (evalState lineScore) $ lines s

p2 :: String -> Int
p2 s = median $ nub $ sort $ map incm $ map (evalState lineScore) $ lines s

main :: Bool -> IO ()
main b = run 10 b p1 p2