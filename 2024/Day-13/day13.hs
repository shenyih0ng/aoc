import Numeric.LinearAlgebra ((><))
import Numeric.LinearAlgebra.Data (Matrix, atIndex)
import Numeric.LinearAlgebra.HMatrix (linearSolve)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

type Config = (Int, Int, Int, Int, Int, Int)

configRegex :: String
configRegex = "Button A: X\\+([0-9]+), Y\\+([0-9]+)\nButton B: X\\+([0-9]+), Y\\+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)"

cost :: Config -> Int
cost (ax, ay, bx, by, x, y) =
  case linearSolve m n of
    Nothing -> 0
    Just result_matrix
      | num_a * ax + num_b * bx == x && num_a * ay + num_b * by == y -> num_a * 3 + num_b
      | otherwise -> 0
      where
        -- `round` is the right way to do it, `ceiling` may overestimate the number of steps (floating point error 🤡)
        (num_a, num_b) = (round $ atIndex result_matrix (0, 0), round $ atIndex result_matrix (1, 0))
  where
    m :: Matrix Double
    m = (2 >< 2) $ map fromIntegral [ax, bx, ay, by]
    n :: Matrix Double
    n = (2 >< 1) $ map fromIntegral [x, y]

main :: IO ()
main = do
  args <- getArgs
  configs_str <- readFile (head args)
  let parsed_configs = configs_str =~ configRegex :: [[String]]
  let configs =
        map
          ( (\[ax, ay, bx, by, x, y] -> (ax, ay, bx, by, x, y))
              . map (\v -> read v :: Int)
              . tail
          )
          parsed_configs
  let corrected_configs =
        map
          ( \(ax, ay, bx, by, x, y) ->
              (ax, ay, bx, by, x + 10000000000000, y + 10000000000000)
          )
          configs
  printf "Part 1: %d\n" $ sum $ map cost configs
  printf "Part 2: %d\n" $ sum $ map cost corrected_configs
