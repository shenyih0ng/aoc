import Data.List (sort)
import System.Environment (getArgs)
import Text.Printf (printf)

type Position = (Int, Int)

data Grid = MkGrid String Int Int

deltas :: [(Int, Int)]
deltas = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]

isValidPosition :: Position -> Grid -> Bool
isValidPosition (i, j) (MkGrid _ n m) = i >= 0 && i < n && j >= 0 && j < m

gatherString :: Grid -> [Position] -> String
gatherString (MkGrid grid n m) path =
  let f p acc = case p of
        [] -> acc
        (i, j) : rest -> f rest (acc ++ [grid !! (i * n + j)])
   in f path ""

getNumXmasAtPosition :: Grid -> Position -> Int
getNumXmasAtPosition g@(MkGrid grid n m) pos@(i, j) =
  let paths = filter (all (`isValidPosition` g)) $ map (\(di, dj) -> [(i + d * di, j + d * dj) | d <- [0 .. 3]]) deltas
   in length $ filter (\path -> gatherString g path == "XMAS") paths

getNumXAtPosition :: Grid -> Position -> Int
getNumXAtPosition g@(MkGrid grid n m) (i, j) =
  if (grid !! (i * n + j) == 'A')
    && ( let coords = [(i - 1, j - 1), (i - 1, j + 1), (i + 1, j - 1), (i + 1, j + 1)]
          in all (`isValidPosition` g) coords
               && let vals@[tlc, trc, blc, bc] = map (\(i, j) -> grid !! (i * n + j)) coords
                   in sort vals == "MMSS" && (tlc == trc || tlc == blc)
       )
    then 1
    else 0

main :: IO ()
main = do
  args <- getArgs
  grid_str <- readFile (head args)
  let n = length $ lines grid_str
  let grid = MkGrid (filter (/= '\n') grid_str) n n
  let coords = [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]
  let num_xmas = sum $ map (getNumXmasAtPosition grid) coords
  let num_x_mas = sum $ map (getNumXAtPosition grid) coords

  printf "Part 1: %d\n" num_xmas
  printf "Part 2: %d\n" num_x_mas