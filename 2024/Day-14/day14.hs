import Data.List (group, sort)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

type Robot = (Int, Int, Int, Int)

roomWidth = 101

roomHeight = 103

robotRegex :: String
robotRegex = "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"

stepN :: Int -> Robot -> Robot
stepN n (px, py, vx, vy) = ((px + (vx * n)) `mod` roomWidth, (py + (vy * n)) `mod` roomHeight, vx, vy)

quad :: Robot -> Int
quad (px, py, _, _)
  | px < roomWidth `div` 2 && py < roomHeight `div` 2 = 1
  | px > roomWidth `div` 2 && py < roomHeight `div` 2 = 2
  | px < roomWidth `div` 2 && py > roomHeight `div` 2 = 3
  | px > roomWidth `div` 2 && py > roomHeight `div` 2 = 4
  | otherwise = 0

freq :: [Int] -> [Int]
freq xs = map length $ group (sort xs)

displayRoom :: [Robot] -> IO ()
displayRoom robots = putStrLn $ unlines room
  where
    positions = Set.fromList $ map (\(px, py, _, _) -> (px, py)) robots
    room = [[if Set.member (x, y) positions then '#' else '.' | x <- [0 .. roomWidth - 1]] | y <- [0 .. roomHeight - 1]]

main :: IO ()
main = do
  args <- getArgs
  robots_str :: [[String]] <- concatMap (=~ robotRegex) . lines <$> readFile (head args)
  let robots :: [Robot] = map ((\[px, py, vx, vy] -> (read px, read py, read vx, read vy)) . tail) robots_str
  printf "Part 1: %d\n" $ product . freq . filter (/= 0) $ map (quad . stepN 100) robots
  -- Strategy for Part 2:
  -- Plot all safety factor values from range 0 to 10000, the first local minimum is the answer
  -- Intuition: Since it forms a tree, the quadrants will be less filled with points as most of the robots congregates at the location
  --            of the tree 🎄
  displayRoom $ map (stepN 7774) robots
