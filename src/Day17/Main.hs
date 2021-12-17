module Day17.Main where

import Data.Bifunctor
import Data.List
import Data.Maybe

input :: ((Int, Int), (Int, Int))
input = ((179, 201), (-109, -63))

p1 :: ((Int, Int), (Int, Int)) -> Int
p1 ((_, _), (y, _)) = div (y * (y + 1)) 2

isTriang :: Int -> Bool
isTriang x = (\k -> k * (k + 1) == 2 * x) $ floor $ sqrt (2 * fromIntegral x :: Float)

isSq :: Int -> Bool
isSq x = (\k -> k * k == x) $ floor $ sqrt (fromIntegral x :: Float)

xReach :: Int -> Int -> Maybe Int
xReach x s
  | isTriang x && s >= floor (sqrt (2 * fromIntegral x :: Float)) = Just $ floor (sqrt (2 * fromIntegral x :: Float))
  | s < x && (mod s 2 == 1 && mod x s == 0 || mod s 2 == 0 && mod x s == div s 2) && s < div (2 * x + s * s - s) (2 * s) = Just $ div (2 * x + s * s - s) (2 * s)
  | otherwise = Nothing

reaches :: (Int, Int) -> [(Int, Int)]
reaches (x, y) =
  let maxSteps = -2 * y
      disc y' = 4 * y' * y' + 4 * y' + 1 - 8 * y - 8 * y'
      negYs = map (\y' -> (y', round (sqrt (fromIntegral (disc y')) / 2 - (2 * fromIntegral y' - 1) / 2))) $ filter (isSq . disc) [1 .. - y]
      posYs = map (\(y', s) -> (y', s + 2 * y' -1)) negYs
      ys = posYs ++ map (first negate) negYs
      xys = map (first fromJust) $ filter (isJust . fst) $ map (\(y', s) -> (xReach x s, y')) ys
   in xys

p2 :: ((Int, Int), (Int, Int)) -> Int
p2 ((xm, xp), (ym, yp)) = length $ nub [k | x <- [xm .. xp], y <- [ym .. yp], k <- reaches (x, y)]

main :: Bool -> IO ()
main False = print $ p1 input
main True = print $ p2 input