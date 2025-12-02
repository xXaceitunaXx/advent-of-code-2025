import System.Environment ( getArgs )
import Data.List.Split (splitOn)

repeatingPattern :: Int -> String -> Bool
repeatingPattern divisor number = number == concat (replicate (div (length number) divisor) (take divisor number))

dividers :: Int -> [Int]
dividers len = [d | d <- [1 .. (div len 2)], (== 0) $ mod len d]

invalid1 :: String -> Bool
invalid1 s 
  | odd (length s) = False
  | otherwise = repeatingPattern (div (length s) 2) s

invalid2 :: String -> Bool
invalid2 s = any (`repeatingPattern` s) (dividers $ length s)

createSequence :: (String, String) -> [String]
createSequence (lower, upper) = map show [ (read lower :: Int) .. (read upper :: Int) ]

parsePairs :: [String] -> [(String, String)]
parsePairs = map (\s -> let [a, b] = splitOn "-" s in (a, b))

main :: IO ()
main = do
 args <- getArgs
 input <- readFile $ head args

 let ids = concatMap createSequence (parsePairs $ splitOn "," input)
 
 print $ sum $ map read $ filter invalid1 ids
 print $ sum $ map read $ filter invalid2 ids