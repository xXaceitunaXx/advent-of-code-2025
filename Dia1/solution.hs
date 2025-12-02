movement :: String -> Int
movement (direction : rest) =
  (case direction of
    'L' -> -1
    'R' -> 1
    _   -> 0) * (read rest :: Int)
movement _ = 0

crossesZero :: Int -> Int -> Int
crossesZero old new
  | new > old = (-) (div new 100) (div old 100)
  | new < old = (-) (div ((-) old 1) 100) (div ((-) new 1) 100)
  | otherwise = 0

main :: IO ()
main = do
 input <- readFile "input"
 let positions = scanl (+) 50 $ map movement $ words input
 let exactos = length $ filter (== 0) $ map (`mod` 100) positions
 let cruces = zipWith crossesZero positions (tail positions)

 print exactos
 print $ sum cruces