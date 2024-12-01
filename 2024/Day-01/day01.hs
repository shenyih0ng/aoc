import Data.List
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Printf (printf)

freqMap :: [Int] -> [(Int, Int)]
freqMap xs = map (\x -> (head x, length x)) . group $ sort xs

getValueInFreqMap :: Int -> [(Int, Int)] -> Int
getValueInFreqMap key freq_map = fromMaybe 0 $ lookup key freq_map

main :: IO ()
main = do
  args <- getArgs
  pairs_str <- map words . lines <$> readFile (head args)
  let pairs = map (\[x, y] -> (read x :: Int, read y :: Int)) pairs_str
  let (xs, ys) = unzip pairs

  -- Part 1
  let (sorted_xs, sorted_ys) = (sort xs, sort ys)
  let diff_acc = sum $ zipWith (\x y -> abs (x - y)) sorted_xs sorted_ys
  -- Part 2
  let (freq_xs, freq_ys) = (freqMap sorted_xs, freqMap sorted_ys)
  let sim_score = sum $ map (\(key, value) -> value * key * getValueInFreqMap key freq_ys) freq_xs

  printf "Part 1: %d\n" diff_acc
  printf "Part 2: %d\n" sim_score
