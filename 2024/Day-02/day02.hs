import Data.List (partition)
import System.Environment (getArgs)
import Text.Printf (printf)

checkAdj :: (Int -> Int -> Bool) -> [Int] -> Bool
checkAdj _ [] = True
checkAdj _ [_] = True
checkAdj f (x : y : xs) = f x y && checkAdj f (y : xs)

isSafe :: [Int] -> Bool
isSafe xs = (checkAdj (>) xs || checkAdj (<) xs) && checkAdj (\x y -> abs (x - y) <= 3 && abs (x - y) >= 1) xs

removeOne :: [Int] -> [[Int]]
removeOne [] = []
removeOne [x] = []
removeOne [x, y] = [[x], [y]]
removeOne (x : xs) = xs : map (x :) (removeOne xs)

main :: IO ()
main = do
  args <- getArgs
  reports_str <- map words . lines <$> readFile (head args)
  let reports = map (map (read :: String -> Int)) reports_str
  let (safe_reports, unsafe_reports) = partition isSafe reports
  let safe_after_remove = filter (any isSafe . removeOne) unsafe_reports
  printf "Part 1: %d\n" (length safe_reports)
  printf "Part 2: %d\n" (length $ safe_reports ++ safe_after_remove)
