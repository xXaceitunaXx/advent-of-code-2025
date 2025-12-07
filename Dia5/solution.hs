import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sortBy)

createSequence :: (String, String) -> (Int, Int)
createSequence (lower, upper) = (read lower, read upper)

parsePairs :: [String] -> [(String, String)]
parsePairs = map (\s -> let [a, b] = splitOn "-" s in (a, b))

inRange :: [(Int, Int)] -> Int -> Bool
inRange ranges number = any (\(lower, upper) -> (number >= lower) && (number <= upper)) ranges

merge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
merge acc (lo, hi) = 
      let (accLo, accHi) = last acc
      in if lo <= accHi + 1 
         then init acc ++ [(accLo, max accHi hi)]
         else acc ++ [(lo, hi)]

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [x] = [x]
mergeRanges ranges = foldl merge [head sorted] (tail sorted)
  where
    sorted = sortBy (\(a, _) (b, _) -> compare a b) ranges

main :: IO ()
main = do
 args <- getArgs
 input <- readFile $ head args

 let [ranges, ingridients] = splitOn [""] $ lines input
     idRanges = map createSequence $ parsePairs ranges

 print $ length $ filter (inRange idRanges . read) ingridients
 print $ sum $ map (\(low, up) -> up - low + 1) $ mergeRanges idRanges 