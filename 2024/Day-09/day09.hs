import Data.Char (digitToInt)
import Data.List (find, partition, sort)
import System.Environment (getArgs)
import Text.Printf (printf)

type Disk = [(Int, Int, Int)] -- [(start_idx, size, file_id)]

scan :: String -> Disk
scan disk_map = go disk_map 0 0 []
  where
    go [] _ _ acc = acc
    go [block_size_c] start_idx file_id acc = acc ++ [(start_idx, digitToInt block_size_c, file_id)]
    go (block_size_c : skip_size_c : rest) start_idx file_id acc =
      let (block_size, skip_size) = (digitToInt block_size_c, digitToInt skip_size_c)
       in go rest (start_idx + block_size + skip_size) (file_id + 1) (acc ++ [(start_idx, block_size, file_id)])

findGapOfSize :: Disk -> Int -> Maybe (Int, Int)
findGapOfSize disk size = go $ zip disk (tail disk)
  where
    go [] = Nothing
    go (((i0, s0, _), (i1, s1, _)) : rest) =
      if i1 - (i0 + s0) >= size
        then Just (i0 + s0, i1 - (i0 + s0))
        else go rest

compact :: Disk -> Disk
compact disk =
  let gap = findGapOfSize disk 1
   in case gap of
        Nothing -> disk
        Just (gap_idx, gap_size) ->
          let last_block@(last_idx, last_size, last_file_id) = last disk
              (before, after) = span (\(idx, _, _) -> idx < gap_idx) $ init disk
           in if last_size <= gap_size
                then
                  compact $ before ++ [(gap_idx, last_size, last_file_id)] ++ after
                else
                  compact $ before ++ [(gap_idx, gap_size, last_file_id)] ++ after ++ [(last_idx, last_size - gap_size, last_file_id)]

compactWhole :: Disk -> Disk
compactWhole disk = let (_, _, last_file_id) = last disk in go disk last_file_id
  where
    go disk 0 = disk
    go disk file_id =
      let file_block = find (\(_, _, f_id) -> f_id == file_id) disk
       in case file_block of
            Nothing -> error "File block not found!"
            Just (idx, block_size, _) ->
              let gap = findGapOfSize disk block_size
               in case gap of
                    Nothing -> if file_id == 0 then disk else go disk (file_id - 1)
                    Just (gap_idx, gap_size) ->
                      if gap_idx >= idx
                        then go disk (file_id - 1)
                        else
                          let intermediate_disk = filter (\(_, _, f_id) -> f_id /= file_id) disk
                              (before, after) = span (\(idx, _, _) -> idx < gap_idx) intermediate_disk
                           in go (before ++ [(gap_idx, block_size, file_id)] ++ after) file_id

checkSum :: Disk -> Int
checkSum disk = sum $ map (\(idx, size, file_id) -> sum [i * file_id | i <- [idx .. idx + size - 1]]) disk

main :: IO ()
main = do
  args <- getArgs
  disk_map_str <- readFile (head args)
  let disk = scan disk_map_str
  printf "Part 1: %d\n" $ checkSum $ compact disk
  printf "Part 2: %d\n" $ checkSum $ compactWhole disk
