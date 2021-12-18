module Day18.Main where

import Data.Maybe
import Harness
import Text.Parsec hiding (Line, State)
import Text.Parsec.String

data Snail = Leaf Int | Node Snail Snail deriving (Eq)

instance Show Snail where
  show (Leaf i) = show i
  show (Node l r) = "[" ++ show l ++ "," ++ show r ++ "]"

nodeParser :: Parser Snail
nodeParser = do
  _ <- char '['
  l <- parser
  _ <- char ','
  r <- parser
  _ <- char ']'
  return $ Node l r

parser :: Parser Snail
parser = nodeParser <|> (Leaf . read <$> many1 digit)

instance Read Snail where
  readsPrec = parsecToReadsPrec parser

snailAdd :: Snail -> Snail -> Snail
snailAdd l r = norm (Node l r)

norm :: Snail -> Snail
norm n =
  let (n', c) = normStep (0, n)
   in if c then norm n' else n'

explodeR :: Snail -> Int -> Snail
explodeR (Leaf n) i = Leaf (n + i)
explodeR (Node l r) i = Node (explodeR l i) r

explodeL :: Snail -> Int -> Snail
explodeL (Leaf n) i = Leaf (n + i)
explodeL (Node l r) i = Node l (explodeL r i)

normStep :: (Int, Snail) -> (Snail, Bool)
normStep (i, n) =
  let (n', c) = expStep (i, n)
   in if isNothing c
        then splitStep (i, n)
        else (n', True)

splitStep :: (Int, Snail) -> (Snail, Bool)
splitStep (_, Leaf n)
  | n > 9 = (Node (Leaf (div n 2)) (Leaf (div (n + 1) 2)), True)
  | otherwise = (Leaf n, False)
splitStep (n, Node l r) =
  let (l', el) = splitStep (n + 1, l)
   in if el
        then (Node l' r, True)
        else
          ( let (r', er) = splitStep (n + 1, r)
             in (Node l' r', er)
          )

expStep :: (Int, Snail) -> (Snail, Maybe (Int, Int))
expStep (4, Node (Leaf l) (Leaf r)) = (Leaf 0, Just (l, r))
expStep (_, Leaf l) = (Leaf l, Nothing)
expStep (n, Node l r) =
  let (l', el) = expStep (n + 1, l)
   in case el of
        Nothing ->
          let (r', er) = expStep (n + 1, r)
           in case er of
                (Just (lv, rv)) -> (Node (explodeL l lv) r', Just (0, rv))
                _ -> (Node l' r', er)
        Just (0, 0) -> (Node l' r, Just (0, 0))
        (Just (lv, rv)) -> (Node l' (explodeR r rv), Just (lv, 0))

mag :: Snail -> Int
mag (Leaf n) = n
mag (Node l r) = 3 * mag l + 2 * mag r

p1 :: [Snail] -> Int
p1 s = mag $ foldl1 snailAdd s

p2 :: [Snail] -> Int
p2 s = maximum [mag (snailAdd l r) | l <- s, r <- s]

main :: Bool -> IO ()
main b = runList 18 b p1 p2