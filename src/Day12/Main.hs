module Day12.Main where

import Control.Applicative hiding (many)
import Data.Char
import Data.List
import qualified Data.Map as M
import Harness
import Text.Parsec hiding (Line, State)
import Text.Parsec.Char
import Text.Parsec.String

data Cave = Start | End | Big {code :: String} | Small {code :: String} deriving (Show, Ord, Eq)

isBig :: Cave -> Bool
isBig (Big _) = True
isBig _ = False

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

data Edge = Edge {a :: Cave, b :: Cave} deriving (Show)

cParser :: Parser Cave
cParser = do
  c <- many letter
  return $ case c of
    "start" -> Start
    "end" -> End
    (h : _)
      | isLower h -> Small c
      | otherwise -> Big c

parser :: Parser Edge
parser = do
  c1 <- cParser
  _ <- char '-'
  c2 <- cParser
  return $ Edge c1 c2

instance Read Edge where
  readsPrec = parsecToReadsPrec parser

adMap :: [Edge] -> M.Map Cave [Cave]
adMap = foldr (\(Edge a b) -> M.insertWith (++) a [b] . M.insertWith (++) b [a]) M.empty

allPaths :: M.Map Cave [Cave] -> [Cave] -> Cave -> Cave -> [[Cave]]
allPaths m seen s e
  | s == e = [[s]]
  | otherwise =
    let succs = m M.! s
     in map (s :) $ (filter (`notElem` seen) succs) >>= (\s' -> allPaths m (if isBig s then seen else s : seen) s' e)

canVisitOS :: [Cave] -> Cave -> Bool
canVisitOS seen c = notElem c seen || (isSmall c && nub seen == seen)

allPathsOS :: M.Map Cave [Cave] -> [Cave] -> Cave -> Cave -> [[Cave]]
allPathsOS m seen s e
  | s == e = [[s]]
  | otherwise =
    let succs = m M.! s
        seen' = if isBig s then seen else s : seen
     in map (s :) $ (filter (canVisitOS seen') succs) >>= (\s' -> allPathsOS m seen' s' e)

p1 :: [Edge] -> Int
p1 s = length $ (\m -> allPaths m [] Start End) $ adMap s

p2 :: [Edge] -> Int
p2 s = length $ (\m -> allPathsOS m [] Start End) $ adMap s

main :: Bool -> IO ()
main b = runList 12 b p1 p2