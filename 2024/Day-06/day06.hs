import Data.List (elemIndex)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)

type Grid = String

type State = (Int, Int, Int, Int) -- (x, y, dx, dy)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (dx, dy) = (dy, -dx)

patrol :: Grid -> Int -> State -> Set.Set (Int, Int)
patrol grid n state@(sx, sy, _, _) =
  let step s@(x, y, dx, dy) visited =
        let nc@(nx, ny) = (x + dx, y + dy)
            ns = (nx, ny, dx, dy)
         in if nx < 0 || ny < 0 || nx >= n || ny >= n
              then visited
              else
                if grid !! (nx * n + ny) == '#'
                  then let (ndx, ndy) = turnRight (dx, dy) in step (x, y, ndx, ndy) visited
                  else step ns (Set.insert (nx, ny) visited)
   in step state (Set.singleton (sx, sy))

patrolLoop :: Grid -> Int -> State -> Set.Set (Int, Int) -> Int
patrolLoop grid n state ob_choices =
  let step ob_coord s@(x, y, dx, dy) seen =
        let nc@(nx, ny) = (x + dx, y + dy)
            ns = (nx, ny, dx, dy)
         in if nx < 0 || ny < 0 || nx >= n || ny >= n
              then 0
              else
                if Set.member ns seen
                  then 1
                  else
                    if nc == ob_coord || grid !! (nx * n + ny) == '#'
                      then let (ndx, ndy) = turnRight (dx, dy) in step ob_coord (x, y, ndx, ndy) seen
                      else step ob_coord ns (Set.insert ns seen)
      empty_coords = Set.toList $ Set.filter (\(x, y) -> grid !! (x * n + y) == '.') ob_choices
   in sum $ map (\ob -> step ob state (Set.singleton state)) empty_coords

main :: IO ()
main = do
  args <- getArgs
  grid_str <- readFile (head args)
  let n = length $ lines grid_str -- Assume that grid is square (n x n)
  let grid = filter (/= '\n') grid_str

  let start_pos = elemIndex '^' grid
  let (sx, sy) = case start_pos of
        Just pos -> (pos `div` n, pos `mod` n)
        Nothing -> error "No starting position found"
  let start_state = (sx, sy, -1, 0)
  let visited_coords = patrol grid n start_state

  printf "Part 1: %d\n" $ Set.size visited_coords
  printf "Part 2: %d\n" $ patrolLoop grid n start_state visited_coords