import Data.Function.Memoize (memoize2)
import System.Environment (getArgs)
import Text.Printf (printf)

numEventually :: Int -> Int -> Int
numEventually val n =
  case (val, n) of
    (_, 0) -> 1
    (0, n) -> numEventuallyMemo 1 (n - 1)
    (x, n) ->
      let (num_str, num_len) = (show x, length num_str)
          (first_num, second_num) = (read (take (num_len `div` 2) num_str) :: Int, read (drop (num_len `div` 2) num_str) :: Int)
       in if even num_len
            then numEventuallyMemo first_num (n - 1) + numEventuallyMemo second_num (n - 1)
            else numEventuallyMemo (x * 2024) (n - 1)

numEventuallyMemo :: Int -> Int -> Int
numEventuallyMemo = memoize2 numEventually

main :: IO ()
main = do
  args <- getArgs
  numbers <- map (\x -> read x :: Int) . words <$> readFile (head args)
  printf "Part 1: %d\n" $ sum $ map (`numEventuallyMemo` 25) numbers
  printf "Part 2: %d\n" $ sum $ map (`numEventuallyMemo` 75) numbers
