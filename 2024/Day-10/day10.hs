import Data.Char (digitToInt)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)

dests :: String -> Int -> (Int, Int) -> [(Int, Int)]
dests grid n coord = traverse coord 0
  where
    dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    nextPositions (x, y) = [(x', y') | (dx, dy) <- dirs, let (x', y') = (x + dx, y + dy), x' >= 0 && x' < n && y' >= 0 && y' < n]
    traverse curr height =
      if height == 9
        then [curr]
        else
          let next = filter (\(x', y') -> digitToInt (grid !! (x' * n + y')) == height + 1) $ nextPositions curr
           in concatMap (\nc -> traverse nc (height + 1)) next

main :: IO ()
main = do
  args <- getArgs
  grid_str <- readFile (head args)
  let n = length $ lines grid_str -- Assume that grid is square (n x n)
  let grid = filter (/= '\n') grid_str
  let trailheads = map (\(i, _) -> (i `div` n, i `mod` n)) $ filter (\(_, c) -> c == '0') $ zip [0 ..] grid
  let end_points = map (dests grid n) trailheads
  printf "Part 1: %d\n" $ sum $ map (Set.size . Set.fromList) end_points
  printf "Part 2: %d\n" $ sum $ map length end_points
