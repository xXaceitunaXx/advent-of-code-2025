import System.Environment ( getArgs )
import Data.List ( tails )

-- jolts :: String -> Int
-- jolts bank = maximum [read [a, b] | (a:rest) <- init (tails bank), b <- rest]

jolts :: Int -> String -> Int
jolts number bank = read (greed number bank)
  where
    greed 0 _ = []
    greed k s = 
      let n = length s
          best = maximum $ take (n - k + 1) s
          (_, rest) = break (== best) s
      in best : greed (k - 1) (tail rest)

main :: IO ()
main = do
 args <- getArgs
 input <- readFile $ head args

 let banks = lines input

 print $ sum $ map (jolts 2) banks
 print $ sum $ map (jolts 12) banks