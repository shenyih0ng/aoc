import System.Environment (getArgs)
import Text.Printf (printf)

concatNum :: Int -> Int -> Int
concatNum a b = read (show a ++ show b) :: Int

isPossible :: Int -> [Int] -> [Int -> Int -> Int] -> Bool
isPossible target nums ops =
  go (head nums) (tail nums)
  where
    go acc rest = case rest of
      [] -> acc == target
      x : xs -> (acc <= target) && any (\op -> go (op acc x) xs) ops

main :: IO ()
main = do
  args <- getArgs
  eq_strs <- lines <$> readFile (head args)
  let eqs = map (\eq_str -> (read (takeWhile (/= ':') eq_str) :: Int, map (\w -> read w :: Int) $ words (drop 1 $ dropWhile (/= ':') eq_str))) eq_strs

  let ops_p1 = [(+), (*)]
  let ops_p2 = [(+), (*), concatNum]
  let getCalibrationResult ops = sum $ map fst $ filter (\(target, nums) -> isPossible target nums ops) eqs

  printf "Part 1: %d\n" $ getCalibrationResult ops_p1
  printf "Part 2: %d\n" $ getCalibrationResult ops_p2