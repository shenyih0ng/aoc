import Data.List (groupBy, partition, sort)
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)

type OrderingMap = Map.Map Int (Set.Set Int)

splitBy :: Char -> String -> [String]
splitBy delimiter str = words $ map (\c -> if c == delimiter then ' ' else c) str

isValidUpdate :: [Int] -> OrderingMap -> Bool
isValidUpdate update come_before_map =
  let checkPos xs = case xs of
        [] -> True
        x : rest -> Set.intersection (Set.fromList rest) (Map.findWithDefault Set.empty x come_before_map) == Set.empty && checkPos rest
   in checkPos update

getCorrectedMiddle :: [Int] -> OrderingMap -> OrderingMap -> Int
getCorrectedMiddle update come_before_map come_after_map =
  let n = length update `div` 2
      hasN ord_set = Set.size (Set.intersection (Set.fromList update) ord_set) == n
      getOrdSet = Map.findWithDefault Set.empty
   in head $ filter (\ele -> hasN (getOrdSet ele come_after_map) || hasN (getOrdSet ele come_after_map)) update

main :: IO ()
main = do
  args <- getArgs
  data_str <- lines <$> readFile (head args)
  let orderings = map (map (\x -> read x :: Int) . splitBy '|') $ takeWhile (not . null) data_str
  let updates = map (map (\x -> read x :: Int) . splitBy ',') $ drop 1 $ dropWhile (not . null) data_str

  let come_after_orderings = groupBy (\x y -> head x == head y) $ sort orderings
  let come_before_orderings = groupBy (\x y -> head x == head y) $ sort $ map reverse orderings
  let mk_ordering_map grps = Map.fromList $ map (\grp -> (head $ head grp, Set.fromList (map (head . tail) grp))) grps

  let [come_after_map, come_before_map] = map mk_ordering_map [come_after_orderings, come_before_orderings]
  let (valid_updates, invalid_updates) = partition (`isValidUpdate` come_before_map) updates

  printf "Part 1: %d\n" $ sum $ map (\update -> update !! (length update `div` 2)) valid_updates
  printf "Part 2: %d\n" $ sum $ map (\update -> getCorrectedMiddle update come_before_map come_after_map) invalid_updates
