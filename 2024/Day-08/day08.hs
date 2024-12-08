import Data.List (groupBy, sort, tails)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

isValidCoord :: Int -> (Int, Int) -> Bool
isValidCoord n (x, y) = x >= 0 && x < n && y >= 0 && y < n

findAntiNodesP1 :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntiNodesP1 ((x1, y1), (x2, y2)) = let (dx, dy) = (x2 - x1, y2 - y1) in [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

findAntiNodesP2 :: ((Int, Int) -> Bool) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntiNodesP2 isValid ((x1, y1), (x2, y2)) =
  let (dx, dy) = (x2 - x1, y2 - y1)
   in takeWhile isValid [(x1 - dx * i, y1 - dy * i) | i <- [0 ..]] ++ takeWhile isValid [(x2 + dx * i, y2 + dy * i) | i <- [0 ..]]

main :: IO ()
main = do
  args <- getArgs
  grid_str <- readFile (head args)
  let n = length $ lines grid_str -- Assume that grid is square (n x n)
  let grid = filter (/= '\n') grid_str

  let antennas = [(v, (x, y)) | x <- [0 .. n - 1], y <- [0 .. n - 1], let v = grid !! (x * n + y), v /= '.']
  let antenna_grps = map (map snd) $ groupBy (\(v, _) (v', _) -> v == v') $ sort antennas
  let antenna_pairs = concatMap uniquePairs antenna_grps

  let numUniqueAntiNodes gen = Set.size $ Set.filter (isValidCoord n) (Set.fromList $ concatMap gen antenna_pairs)

  printf "Part 1: %d\n" $ numUniqueAntiNodes findAntiNodesP1
  printf "Part 2: %d\n" $ numUniqueAntiNodes (findAntiNodesP2 $ isValidCoord n)
