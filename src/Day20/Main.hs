module Day20.Main where

import Harness

parse :: String -> ([Bool], [[Bool]])
parse s =
  let ls = lines s
      rules = map (== '#') $ head ls
      grid = map (map (== '#')) $ drop 2 ls
   in (rules, grid)

rep :: [Bool] -> ((Bool, Bool, Bool), (Bool, Bool, Bool), (Bool, Bool, Bool)) -> Bool
rep rs ((a, b, c), (d, e, f), (g, h, i)) =
  let idx = foldl (\a b -> 2 * a + if b then 1 else 0) 0 [a, b, c, d, e, f, g, h, i]
   in rs !! idx

step :: [Bool] -> (Bool, [[Bool]]) -> (Bool, [[Bool]])
step rules (b, grid) =
  let hneighs = map (\r -> zip3 (b : b : r) (b : r ++ [b]) (r ++ [b, b])) grid
      neighs = zipWith3 zip3 (repeat (b, b, b) : repeat (b, b, b) : hneighs) (repeat (b, b, b) : hneighs ++ [repeat (b, b, b)]) (hneighs ++ [repeat (b, b, b), repeat (b, b, b)])
   in (not b, map (map (rep rules)) neighs)

gShow :: [[Bool]] -> IO ()
gShow = mapM_ (putStrLn . map (\x -> if x then '#' else ' '))

testRule :: [Bool]
testRule = map (== '#') "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"

testGrid :: [[Bool]]
testGrid = map (map (== '#')) ["#..#.", "#....", "##..#", "..#..", "..###"]

p1 :: String -> IO ()
p1 s = do
  let (rules, grid) = parse s
  let res = iterate (step rules) (False, grid) !! 2
  gShow $ snd res
  print $ sum $ map (sum . map (\x -> if x then 1 else 0)) $ snd res

p2 :: String -> IO ()
p2 s = do
  let (rules, grid) = parse s
  let res = iterate (step rules) (False, grid) !! 50
  gShow $ snd res
  print $ sum $ map (sum . map (\x -> if x then 1 else 0)) $ snd res

main :: Bool -> IO ()
main b = runIO 20 b p1 p2