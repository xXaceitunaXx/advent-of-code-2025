import System.Environment ( getArgs )

type Grid = [String]

countRolls :: Grid -> Int
countRolls rows = length $ concatMap (filter (== '@')) rows

getCell :: Grid -> Int -> Int -> Char
getCell grid x y
  | x < 0 || y < 0 || y >= length grid || x >= length (head grid) = '.'
  | otherwise = (grid !! y) !! x

getNeighborsData :: Grid -> [(Int, Int, Int)]
getNeighborsData grid =
     let neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

         countNeighbors _ '.' = maxBound
         countNeighbors ns _  = length $ filter (== '@') ns

     in [ (countNeighbors [getCell grid nx ny | (nx, ny) <- neighbors (x, y)] c, x, y)
        | (y, row) <- zip [0..] grid, (x, c) <- zip [0..] row ]

updateGrid :: Grid -> [(Int, Int, Int)] -> Grid
updateGrid grid toRemove =
    let targets = [(x, y) | (_, x, y) <- toRemove]
        isTarget x y = (x, y) `elem` targets
    in [[ if isTarget x y then '.' else c | (x, c) <- zip [0..] row ] | (y, row) <- zip [0..] grid]

solve :: Grid -> Grid
solve grid =
    let candidates = filter (\(n, _, _) -> n < 4) (getNeighborsData grid)
    in if null candidates
       then grid
       else solve (updateGrid grid candidates)

main :: IO ()
main = do
 args <- getArgs
 input <- readFile $ head args

 let rows = lines input
 print $ length $ filter (\(n, _, _) -> n < 4) $ getNeighborsData rows

 print $ (-) (countRolls rows) (countRolls $ solve rows)