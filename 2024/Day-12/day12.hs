{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonadComprehensions #-}

import Data.Bifunctor (bimap)
import Data.Ix (inRange)
import Data.List (filter, unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)

type Coord = (Int, Int)

type Garden = Map Coord Char

adjCoords :: Coord -> [Coord]
adjCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isValid :: Coord -> Int -> Bool
isValid (x, y) n = inRange (0, n - 1) x && inRange (0, n - 1) y

dfs :: Int -> Garden -> Coord -> Char -> Set Coord
dfs n garden start target = go Set.empty [start]
  where
    go visited [] = visited
    go visited (curr : rest) =
      let visited' = Set.insert curr visited
          next =
            filter (\pos -> isValid pos n && Map.lookup pos garden == Just target && not (Set.member pos visited')) $
              adjCoords curr
       in go visited' (next ++ rest)

regions :: Int -> Map Coord Char -> [Set Coord]
regions n = unfoldr \garden ->
  [ (region, Map.withoutKeys garden region)
    | (start, value) <- Map.lookupMin garden,
      let region = dfs n garden start value
  ]

edges :: Set Coord -> Set (Coord, Coord) -- Set (pos, direction)
edges region =
  Set.fromList
    [ (pos, bimap (fst adjPos -) (snd adjPos -) pos)
      | pos <- Set.toList region,
        adjPos <- adjCoords pos,
        not (Set.member adjPos region)
    ]

-- Intuition: Choose a single edge to represent a single side of the region
-- The edge should be the one that is at the end/start of the side
numSides :: Set (Coord, Coord) -> Int
numSides edges =
  length
    $ filter
      ( \(pos, dir) ->
          let dir_turn_right = (-(snd dir), fst dir)
              test_pos = bimap (fst pos +) (snd pos +) dir_turn_right
           in not (Set.member (test_pos, dir) edges)
      )
    $ Set.toList edges

main :: IO ()
main = do
  args <- getArgs
  garden_str <- readFile (head args)
  let n = length $ lines garden_str -- Assume that grid is square (n x n)
  let garden = Map.fromAscList $ zipWith (\i v -> ((i `div` n, i `mod` n), v)) [0 ..] (filter (/= '\n') garden_str)
  let garden_regions = regions n garden
  let region_edges = map edges garden_regions
  printf "Part 1: %d\n" $ sum $ zipWith (\r e -> length r * Set.size e) garden_regions region_edges
  printf "Part 2: %d\n" $ sum $ zipWith (\r e -> length r * numSides e) garden_regions region_edges
