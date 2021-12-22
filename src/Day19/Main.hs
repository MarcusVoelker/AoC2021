module Day19.Main where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Harness

data Heading = None | Ax Axis R90 | Fac Face | Diag Diagonal R120 deriving (Show, Eq)

data Axis = X | Y | Z deriving (Show, Eq)

data Face = XYP | XYN | XZP | XZN | YZP | YZN deriving (Show, Eq)

data Diagonal = XYZ | XyZ | XYz | Xyz deriving (Show, Eq)

data R90 = R90 | R180 | R270 deriving (Show, Eq)

data R120 = R120 | R240 deriving (Show, Eq)

invert :: Heading -> Heading
invert (Ax a R90) = Ax a R270
invert (Ax a R270) = Ax a R90
invert (Diag d R120) = Diag d R240
invert (Diag d R240) = Diag d R120
invert h = h

readMany :: (Read a) => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

parseScanner :: [String] -> [(Int, Int, Int)]
parseScanner s =
  let ls = tail s
   in map ((\[a, b, c] -> (a, b, c)) . (readMany :: String -> [Int])) ls

parse :: String -> [[(Int, Int, Int)]]
parse s =
  let ls = lines s
      scan [] = []
      scan xs = (\(s, r) -> parseScanner s : (if null r then [] else scan (tail r))) $ break null xs
   in scan ls

sin90 :: R90 -> Int
sin90 R90 = 1
sin90 R180 = 0
sin90 R270 = -1

sin120 :: R120 -> Int
sin120 R120 = 1
sin120 R240 = -1

cos90 :: R90 -> Int
cos90 R90 = 0
cos90 R180 = -1
cos90 R270 = 0

mat :: Heading -> ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))
mat None = ((1, 0, 0), (0, 1, 0), (0, 0, 1))
mat (Ax a r) =
  let sr = sin90 r
      cr = cos90 r
      ux = if a == X then 1 else 0
      uy = if a == Y then 1 else 0
      uz = if a == Z then 1 else 0
   in ( (cr + ux * (1 - cr), - uz * sr, uy * sr),
        (uz * sr, cr + uy * (1 - cr), - ux * sr),
        (- uy * sr, ux * sr, cr + uz * (1 - cr))
      )
mat (Fac a) =
  let ux = if a `elem` [XYP, XYN, XZP, XZN] then 1 else 0
      uy
        | a `elem` [XYP, YZP, YZN] = 1
        | a == XYN = -1
        | otherwise = 0
      uz
        | a `elem` [XZP, YZP] = 1
        | a `elem` [XZN, YZN] = -1
        | otherwise = 0
   in ( (ux * ux -1, ux * uy, ux * uz),
        (ux * uy, uy * uy -1, uy * uz),
        (ux * uz, uy * uz, uz * uz -1)
      )
mat (Diag a r) =
  let sr = sin120 r
      ux = 1
      uy = if a `elem` [XYZ, XYz] then 1 else -1
      uz = if a `elem` [XYZ, XyZ] then 1 else -1
   in ( (0, div (ux * uy - uz * sr) 2, div (ux * uz + uy * sr) 2),
        (div (ux * uy + uz * sr) 2, 0, div (uy * uz - ux * sr) 2),
        (div (ux * uz - uy * sr) 2, div (uy * uz + ux * sr) 2, 0)
      )

mul :: ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int)) -> (Int, Int, Int) -> (Int, Int, Int)
mul ((a, b, c), (d, e, f), (g, h, i)) (x, y, z) = (a * x + b * y + c * z, d * x + e * y + f * z, g * x + h * y + i * z)

positionIn :: (Int, Int, Int) -> (Int, Int, Int) -> Heading -> (Int, Int, Int)
positionIn (x0, y0, z0) p1 h =
  let tm1 = mat (invert h)
      (x1, y1, z1) = mul tm1 p1
   in (x0 - x1, y0 - y1, z0 - z1)

position :: (Int, Int, Int) -> (Int, Int, Int) -> [((Int, Int, Int), Heading)]
position p0 p1 = map (\h -> (positionIn p0 p1 h, h)) $ [None] ++ [Ax a r | a <- [X, Y, Z], r <- [R90, R180, R270]] ++ [Fac f | f <- [XYP, XYN, XZP, XZN, YZP, YZN]] ++ [Diag a r | a <- [XYZ, XyZ, XYz, Xyz], r <- [R120, R240]]

transform :: ((Int, Int, Int), Heading) -> (Int, Int, Int) -> (Int, Int, Int)
transform ((px, py, pz), h) (ox, oy, oz) = mul (mat h) (ox - px, oy - py, oz - pz)

itransform :: ((Int, Int, Int), Heading) -> (Int, Int, Int) -> (Int, Int, Int)
itransform ((px, py, pz), h) o = (\(ox, oy, oz) -> (ox + px, oy + py, oz + pz)) $ mul (mat (invert h)) o

overlap :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Maybe ((Int, Int, Int), Heading)
overlap n ba bb
  | length ba < n = Nothing
  | otherwise =
    let p0 = head ba
        rs = nub $ concatMap (position p0) bb
        ts = filter (\b -> length (snd b `intersect` ba) >= n) $ map (\r -> (r, map (itransform r) bb)) rs
     in if null ts
          then overlap n (tail ba) bb
          else Just $ fst (head ts)

merge :: [[(Int, Int, Int)]] -> [((Int, Int), ((Int, Int, Int), Heading))] -> [Int] -> Int -> [(Int, Int, Int)]
merge ss m seen cur
  | cur `elem` seen = []
  | otherwise =
    let fsuccs = map (\((_, s), t) -> (s, t)) $ filter (\((i, _), _) -> i == cur) m
        rsuccs = map (\((s, _), t) -> (s, t)) $ filter (\((_, i), _) -> i == cur) m
        fvals = concatMap (\(s, t) -> map (itransform t) $ merge ss m (cur : seen) s) fsuccs
        rvals = concatMap (\(s, t) -> map (transform t) $ merge ss m (cur : seen) s) rsuccs
     in nub ((ss !! cur) ++ fvals ++ rvals)

mergeScanners :: [((Int, Int), ((Int, Int, Int), Heading))] -> [Int] -> Int -> [(Int, Int, Int)]
mergeScanners m seen cur
  | cur `elem` seen = []
  | otherwise =
    let fsuccs = map (\((_, s), t) -> (s, t)) $ filter (\((i, _), _) -> i == cur) m
        rsuccs = map (\((s, _), t) -> (s, t)) $ filter (\((_, i), _) -> i == cur) m
        fvals = concatMap (\(s, t) -> map (itransform t) $ mergeScanners m (cur : seen) s) fsuccs
        rvals = concatMap (\(s, t) -> map (transform t) $ mergeScanners m (cur : seen) s) rsuccs
     in nub ((0, 0, 0) : fvals ++ rvals)

reconstruct :: Int -> [[(Int, Int, Int)]] -> IO [(Int, Int, Int)]
reconstruct n ss = do
  let zs = zip [0 ..] ss
  let res = catMaybes [((i, j),) <$> overlap n a b | (i, a) <- zs, (j, b) <- zs, i < j]
  print res
  return $ merge ss res [] 0

reconstructScanners :: Int -> [[(Int, Int, Int)]] -> IO [(Int, Int, Int)]
reconstructScanners n ss = do
  let zs = zip [0 ..] ss
  let res = catMaybes [((i, j),) <$> overlap n a b | (i, a) <- zs, (j, b) <- zs, i < j]
  print res
  return $ mergeScanners res [] 0

p1 :: String -> IO ()
p1 s = do
  res <- reconstruct 12 $ parse s
  print res
  print $ length res

manhattan :: (Int, Int, Int) -> (Int, Int, Int) -> Int
manhattan (ax, ay, az) (bx, by, bz) = abs (ax - bx) + abs (ay - by) + abs (az - bz)

p2 :: String -> IO ()
p2 s = do
  res <- reconstructScanners 12 $ parse s
  print res
  print $ maximum [manhattan l r | l <- res, r <- res]

main :: Bool -> IO ()
main b = runIO 19 b p1 p2