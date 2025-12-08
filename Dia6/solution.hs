import System.Environment (getArgs)
import Data.List (transpose)
import Data.List.Split (splitOn)

operate :: [String] -> Int
operate operation = case last operation of
 "+" -> sum $ map read $ init operation
 "*" -> product $ map read $ init operation

parse :: [String] -> [[String]]
parse input = zipWith (\nums op -> nums ++ [op]) problems operations
    where numbers = init input
          operations = words $ last input
          problems = map words $ splitOn "  " $ unwords $ map (filter (/= ' ')) $ transpose numbers

main :: IO ()
main = do
 args <- getArgs
 input <- readFile $ head args

 print $ sum $ map operate $ transpose $ map words $ lines input
 print $ sum $ map operate $ parse $ lines input