import Data.Maybe
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.Posix

mulExpRegex :: String
mulExpRegex = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

parseInt :: String -> Int -> Int
parseInt str def = Data.Maybe.fromMaybe def (readMaybe str :: Maybe Int)

execMulExp :: [String] -> Int
execMulExp [_, n1, n2] = parseInt n1 0 * parseInt n2 0

sumEnabled :: [[String]] -> Bool -> Int
sumEnabled exps ignore =
  case exps of
    [] -> 0
    (("do()" : _) : rest) -> sumEnabled rest False
    (("don't()" : _) : rest) -> sumEnabled rest True
    (mulExp : rest) -> if ignore then sumEnabled rest ignore else execMulExp mulExp + sumEnabled rest ignore

main :: IO ()
main = do
  args <- getArgs
  mem_str <- readFile (head args)
  let matches = mem_str =~ mulExpRegex :: [[String]]
  let result = sum $ map execMulExp matches
  let result_enabled = sumEnabled matches False
  printf "Part 1: %d\n" result
  printf "Part 2: %d\n" result_enabled
